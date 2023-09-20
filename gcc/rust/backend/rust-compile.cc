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

#include "rust-compile.h"
#include "libproc_macro_internal/proc_macro.h"
#include "rust-compile-item.h"
#include "rust-compile-implitem.h"
#include "rust-hir-type-bounds.h"
#include "rust-compile-type.h"
#include "rust-substitution-mapper.h"
#include "rust-type-util.h"
#include "rust-session-manager.h"

namespace Rust {
namespace Compile {

const std::string GCCRS_PROC_MACRO_SYMBOL_PREFIX = "__gccrs_proc_macro_";

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

// This namespace brings multiple function to build and initialize multiple
// structures that needs to get exposed in the final shared object binary.
namespace {

tree
build_attribute_array (std::vector<std::string> attributes)
{
  tree attribute_ptr = build_pointer_type (char_type_node);
  tree attribute_type = build_qualified_type (attribute_ptr, TYPE_QUAL_CONST);
  return build_array_type_nelts (attribute_type, attributes.size ());
}

// We're constructing the following structure:
//
// struct {
//     const char *trait_name;
//     const char **attributes;
//     std::uint64_t attr_size;
//     TokenStream (fndecl*) (TokenStream);
// }
tree
build_derive_proc_macro ()
{
  tree char_ptr = build_pointer_type (char_type_node);
  tree const_char_type = build_qualified_type (char_ptr, TYPE_QUAL_CONST);
  Backend::typed_identifier name_field
    = Backend::typed_identifier ("trait_name", const_char_type,
				 BUILTINS_LOCATION);

  tree handle_ptr = build_pointer_type (void_type_node);
  Backend::typed_identifier fndecl_field
    = Backend::typed_identifier ("fndecl", handle_ptr, BUILTINS_LOCATION);

  tree attribute_ptr = build_pointer_type (const_ptr_type_node);
  Backend::typed_identifier attributes_field
    = Backend::typed_identifier ("attributes", attribute_ptr,
				 BUILTINS_LOCATION);

  Backend::typed_identifier size_field
    = Backend::typed_identifier ("attr_size", unsigned_type_node,
				 BUILTINS_LOCATION);

  return Backend::struct_type (
    {name_field, attributes_field, size_field, fndecl_field});
}

// We're constructing the following structure:
//
// struct {
//     const char *name;
//     TokenStream (fndecl*) (TokenStream);
// }
tree
build_bang_proc_macro ()
{
  tree char_ptr = build_pointer_type (char_type_node);
  tree const_char_type = build_qualified_type (char_ptr, TYPE_QUAL_CONST);
  Backend::typed_identifier name_field
    = Backend::typed_identifier ("name", const_char_type, BUILTINS_LOCATION);

  tree handle_ptr = ptr_type_node;
  Backend::typed_identifier fndecl_field
    = Backend::typed_identifier ("fndecl", handle_ptr, BUILTINS_LOCATION);

  return Backend::struct_type ({name_field, fndecl_field});
}

// Bang proc macros and attribute proc macros almost have the same members
// the function pointer type is not the same.
//
// We're constructing the following structure:
//
// struct {
//     const char *name;
//     TokenStream (fndecl*) (TokenStream, TokenStream);
// }
tree
build_attribute_proc_macro ()
{
  return build_bang_proc_macro ();
}

tree
build_proc_macro_payload ()
{
  tree bang = build_bang_proc_macro ();
  tree attribute = build_attribute_proc_macro ();
  tree derive = build_derive_proc_macro ();

  Backend::typed_identifier bang_field
    = Backend::typed_identifier ("bang", bang, BUILTINS_LOCATION);
  Backend::typed_identifier attribute_field
    = Backend::typed_identifier ("attribute", attribute, BUILTINS_LOCATION);
  Backend::typed_identifier derive_field
    = Backend::typed_identifier ("custom_derive", derive, BUILTINS_LOCATION);

  // We rely on the tag to represent the index of any union member. This means
  // we should keep those fields in the same order as the tag representation for
  // it to be kept in sync.
  // Hence why the following code exist: to keep in sync the field vector and
  // the tag enumeration.
  std::vector<Backend::typed_identifier> fields;
  fields.insert (fields.begin () + ProcMacro::CUSTOM_DERIVE, derive_field);
  fields.insert (fields.begin () + ProcMacro::ATTR, attribute_field);
  fields.insert (fields.begin () + ProcMacro::BANG, bang_field);

  return Backend::union_type (fields);
}

// Build the tagged union proc macro type
//
// struct {
//     unsigned short tag;
//     union { BangProcMacro , DeriveProcMacro, AttributeProcMacro} payload;
// }
tree
build_proc_macro ()
{
  auto union_field = build_proc_macro_payload ();
  Backend::typed_identifier payload_field
    = Backend::typed_identifier ("payload", union_field, BUILTINS_LOCATION);

  Backend::typed_identifier tag_field
    = Backend::typed_identifier ("tag", short_unsigned_type_node,
				 BUILTINS_LOCATION);

  return Backend::struct_type ({tag_field, payload_field});
}

tree
build_proc_macro_buffer (tree proc_macro_type, size_t total_macro)
{
  return build_array_type_nelts (proc_macro_type, total_macro);
}

tree
build_entrypoint (tree proc_macro_buffer)
{
  return build_reference_type_for_mode (proc_macro_buffer, E_VOIDmode, false);
}

tree
init_derive_proc_macro (Context *ctx, CustomDeriveInfo infos)
{
  tree derive_proc_macro_type = build_derive_proc_macro ();
  tree trait_name = build_string_literal (infos.trait_name.c_str ());

  tree attribute_ptr;
  if (infos.attributes.size () == 0)
    {
      attribute_ptr = HIRCompileBase::address_expression (null_pointer_node,
							  BUILTINS_LOCATION);
    }
  else
    {
      tree attribute_array_type = build_attribute_array (infos.attributes);

      std::vector<tree> attr_ctors;
      std::vector<unsigned long> indices;

      size_t index = 0;
      for (auto &attr : infos.attributes)
	{
	  attr_ctors.push_back (build_string_literal (attr.c_str ()));
	  indices.push_back (index);
	  index++;
	}

      tree attributes
	= Backend::array_constructor_expression (attribute_array_type, indices,
						 attr_ctors, BUILTINS_LOCATION);

      std::string attribute_var_name
	= GCCRS_PROC_MACRO_SYMBOL_PREFIX + infos.trait_name;
      Bvariable *attributes_var
	= Backend::global_variable (attribute_var_name.c_str (),
				    attribute_var_name.c_str (),
				    attribute_array_type, false /* internal */,
				    true /* hidden */, false /* no gc */,
				    BUILTINS_LOCATION);
      Backend::global_variable_set_init (attributes_var, attributes);
      ctx->push_var (attributes_var);

      attribute_ptr
	= HIRCompileBase::address_expression (attributes_var->get_decl (),
					      BUILTINS_LOCATION);
    }

  tree attr_size = build_int_cst (unsigned_type_node, infos.attributes.size ());

  tree handle
    = HIRCompileBase::address_expression (infos.fndecl, BUILTINS_LOCATION);

  return Backend::constructor_expression (derive_proc_macro_type, false,
					  {trait_name, attribute_ptr, attr_size,
					   handle},
					  -1 /* Structure: no index */,
					  BUILTINS_LOCATION);
}

tree
init_attribute_proc_macro (tree macro)
{
  tree attribute_proc_macro_type = build_attribute_proc_macro ();
  tree macro_name
    = build_string_literal (IDENTIFIER_POINTER (DECL_NAME (macro)));
  tree handle = HIRCompileBase::address_expression (macro, BUILTINS_LOCATION);

  return Backend::constructor_expression (attribute_proc_macro_type, false,
					  {macro_name, handle},
					  -1 /* Structure: No index */,
					  BUILTINS_LOCATION);
}

tree
init_bang_proc_macro (tree macro)
{
  // Attribute and bang proc macros have the same structure, they can be
  // initialized with the same code.
  return init_attribute_proc_macro (macro);
}

tree
init_proc_macro (tree payload, tree proc_macro_type,
		 ProcMacro::ProcmacroTag tag)
{
  auto discriminant = static_cast<int> (tag);

  tree macro_tag = build_int_cst (short_unsigned_type_node, discriminant);

  tree payload_union
    = Backend::constructor_expression (build_proc_macro_payload (), false,
				       {payload},
				       discriminant /* Union: member index */,
				       BUILTINS_LOCATION);

  return Backend::constructor_expression (proc_macro_type,
					  false /* invariant */,
					  {macro_tag, payload_union},
					  -1 /* Structure: No index */,
					  BUILTINS_LOCATION);
}

tree
initialize_proc_macro_array (Context *ctx, tree proc_macro_buffer_type,
			     tree proc_macro_type)
{
  std::vector<unsigned long> indexes;
  std::vector<tree> ctors;
  size_t index = 0;
  for (auto &macro : ctx->get_derive_proc_macros ())
    {
      tree proc_macro = init_derive_proc_macro (ctx, macro);
      ctors.push_back (
	init_proc_macro (proc_macro, proc_macro_type,
			 ProcMacro::ProcmacroTag::CUSTOM_DERIVE));
      indexes.push_back (index);
      index++;
    }
  for (auto &macro : ctx->get_attribute_proc_macros ())
    {
      tree proc_macro = init_attribute_proc_macro (macro);

      ctors.push_back (init_proc_macro (proc_macro, proc_macro_type,
					ProcMacro::ProcmacroTag::ATTR));
      indexes.push_back (index);
      index++;
    }
  for (auto &macro : ctx->get_bang_proc_macros ())
    {
      tree proc_macro = init_bang_proc_macro (macro);

      ctors.push_back (init_proc_macro (proc_macro, proc_macro_type,
					ProcMacro::ProcmacroTag::BANG));
      indexes.push_back (index);
      index++;
    }

  return Backend::array_constructor_expression (proc_macro_buffer_type, indexes,
						ctors, BUILTINS_LOCATION);
}

} // namespace

void
CompileCrate::add_proc_macro_symbols ()
{
  auto total_macros = ctx->get_attribute_proc_macros ().size ()
		      + ctx->get_bang_proc_macros ().size ()
		      + ctx->get_derive_proc_macros ().size ();

  tree proc_macro_type = build_proc_macro ();
  tree proc_macro_buffer_type
    = build_proc_macro_buffer (proc_macro_type, total_macros);
  tree entrypoint_type = build_entrypoint (proc_macro_buffer_type);

  std::string decl_symbol_name = generate_proc_macro_decls_symbol (
    0 /* FIXME: Change to stable crate id */);

  Bvariable *macro_decls
    = Backend::global_variable (decl_symbol_name.c_str (),
				decl_symbol_name.c_str (), entrypoint_type,
				false /* internal */, false /* not hidden */,
				false /* no gc */, BUILTINS_LOCATION);

  std::string buffer_name
    = GCCRS_PROC_MACRO_SYMBOL_PREFIX + "proc_macro_buffer";

  Bvariable *proc_macro_buffer
    = Backend::global_variable (buffer_name.c_str (), buffer_name.c_str (),
				proc_macro_buffer_type, false /* internal */,
				true /* hidden */, false /* no gc */,
				BUILTINS_LOCATION);
  Backend::global_variable_set_init (
    proc_macro_buffer,
    initialize_proc_macro_array (ctx, proc_macro_buffer_type, proc_macro_type));
  ctx->push_var (proc_macro_buffer);

  Backend::global_variable_set_init (
    macro_decls,
    HIRCompileBase::address_expression (proc_macro_buffer->get_decl (),
					BUILTINS_LOCATION));

  ctx->push_var (macro_decls);
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
      rvalue = resolve_adjustements (*adjustments, rvalue, rvalue_locus);
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
HIRCompileBase::coerce_to_dyn_object (tree compiled_ref,
				      const TyTy::BaseType *actual,
				      const TyTy::DynamicObjectType *ty,
				      location_t locus)
{
  // DST's get wrapped in a pseudo reference that doesnt exist...
  const TyTy::ReferenceType r (ctx->get_mappings ()->get_next_hir_id (),
			       TyTy::TyVar (ty->get_ref ()), Mutability::Imm);

  tree dynamic_object = TyTyResolveCompile::compile (ctx, &r);
  tree dynamic_object_fields = TYPE_FIELDS (dynamic_object);
  tree vtable_field = DECL_CHAIN (dynamic_object_fields);
  rust_assert (TREE_CODE (TREE_TYPE (vtable_field)) == ARRAY_TYPE);

  //' this assumes ordering and current the structure is
  // __trait_object_ptr
  // [list of function ptrs]

  std::vector<std::pair<Resolver::TraitReference *, HIR::ImplBlock *>>
    probed_bounds_for_receiver = Resolver::TypeBoundsProbe::Probe (actual);

  tree address_of_compiled_ref = null_pointer_node;
  if (!actual->is_unit ())
    address_of_compiled_ref = address_expression (compiled_ref, locus);

  std::vector<tree> vtable_ctor_elems;
  std::vector<unsigned long> vtable_ctor_idx;
  unsigned long i = 0;
  for (auto &bound : ty->get_object_items ())
    {
      const Resolver::TraitItemReference *item = bound.first;
      const TyTy::TypeBoundPredicate *predicate = bound.second;

      auto address = compute_address_for_trait_item (item, predicate,
						     probed_bounds_for_receiver,
						     actual, actual, locus);
      vtable_ctor_elems.push_back (address);
      vtable_ctor_idx.push_back (i++);
    }

  tree vtable_ctor
    = Backend::array_constructor_expression (TREE_TYPE (vtable_field),
					     vtable_ctor_idx, vtable_ctor_elems,
					     locus);

  std::vector<tree> dyn_ctor = {address_of_compiled_ref, vtable_ctor};
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
  // There are two cases here one where its an item which has an implementation
  // within a trait-impl-block. Then there is the case where there is a default
  // implementation for this within the trait.
  //
  // The awkward part here is that this might be a generic trait and we need to
  // figure out the correct monomorphized type for this so we can resolve the
  // address of the function , this is stored as part of the
  // type-bound-predicate
  //
  // Algo:
  // check if there is an impl-item for this trait-item-ref first
  // else assert that the trait-item-ref has an implementation
  //
  // FIXME this does not support super traits

  TyTy::TypeBoundPredicateItem predicate_item
    = predicate->lookup_associated_item (ref->get_identifier ());
  rust_assert (!predicate_item.is_error ());

  // this is the expected end type
  TyTy::BaseType *trait_item_type = predicate_item.get_tyty_for_receiver (root);
  rust_assert (trait_item_type->get_kind () == TyTy::TypeKind::FNDEF);
  TyTy::FnType *trait_item_fntype
    = static_cast<TyTy::FnType *> (trait_item_type);

  // find impl-block for this trait-item-ref
  HIR::ImplBlock *associated_impl_block = nullptr;
  const Resolver::TraitReference *predicate_trait_ref = predicate->get ();
  for (auto &item : receiver_bounds)
    {
      Resolver::TraitReference *trait_ref = item.first;
      HIR::ImplBlock *impl_block = item.second;
      if (predicate_trait_ref->is_equal (*trait_ref))
	{
	  associated_impl_block = impl_block;
	  break;
	}
    }

  // FIXME this probably should just return error_mark_node but this helps
  // debug for now since we are wrongly returning early on type-resolution
  // failures, until we take advantage of more error types and error_mark_node
  rust_assert (associated_impl_block != nullptr);

  // lookup self for the associated impl
  std::unique_ptr<HIR::Type> &self_type_path
    = associated_impl_block->get_type ();
  TyTy::BaseType *self = nullptr;
  bool ok = ctx->get_tyctx ()->lookup_type (
    self_type_path->get_mappings ().get_hirid (), &self);
  rust_assert (ok);

  // lookup the predicate item from the self
  TyTy::TypeBoundPredicate *self_bound = nullptr;
  for (auto &bound : self->get_specified_bounds ())
    {
      const Resolver::TraitReference *bound_ref = bound.get ();
      const Resolver::TraitReference *specified_ref = predicate->get ();
      if (bound_ref->is_equal (*specified_ref))
	{
	  self_bound = &bound;
	  break;
	}
    }
  rust_assert (self_bound != nullptr);

  // lookup the associated item from the associated impl block
  TyTy::TypeBoundPredicateItem associated_self_item
    = self_bound->lookup_associated_item (ref->get_identifier ());
  rust_assert (!associated_self_item.is_error ());

  // Lookup the impl-block for the associated impl_item if it exists
  HIR::Function *associated_function = nullptr;
  for (auto &impl_item : associated_impl_block->get_impl_items ())
    {
      bool is_function = impl_item->get_impl_item_type ()
			 == HIR::ImplItem::ImplItemType::FUNCTION;
      if (!is_function)
	continue;

      HIR::Function *fn = static_cast<HIR::Function *> (impl_item.get ());
      bool found_associated_item
	= fn->get_function_name ().as_string ().compare (ref->get_identifier ())
	  == 0;
      if (found_associated_item)
	associated_function = fn;
    }

  // we found an impl_item for this
  if (associated_function != nullptr)
    {
      // lookup the associated type for this item
      TyTy::BaseType *lookup = nullptr;
      bool ok = ctx->get_tyctx ()->lookup_type (
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
					       lookup_fntype, true, locus);
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
