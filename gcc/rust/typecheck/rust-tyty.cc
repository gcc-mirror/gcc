// Copyright (C) 2020-2025 Free Software Foundation, Inc.

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

#include "rust-tyty.h"

#include "optional.h"
#include "rust-tyty-visitor.h"
#include "rust-hir-map.h"
#include "rust-location.h"
#include "rust-linemap.h"

#include "rust-substitution-mapper.h"
#include "rust-hir-trait-reference.h"
#include "rust-hir-trait-resolve.h"
#include "rust-tyty-cmp.h"
#include "rust-type-util.h"
#include "rust-hir-type-bounds.h"

#include "options.h"
#include "rust-system.h"

namespace Rust {
namespace TyTy {

std::string
TypeKindFormat::to_string (TypeKind kind)
{
  switch (kind)
    {
    case TypeKind::INFER:
      return "Infer";

    case TypeKind::ADT:
      return "ADT";

    case TypeKind::STR:
      return "STR";

    case TypeKind::REF:
      return "REF";

    case TypeKind::POINTER:
      return "POINTER";

    case TypeKind::PARAM:
      return "PARAM";

    case TypeKind::ARRAY:
      return "ARRAY";

    case TypeKind::SLICE:
      return "SLICE";

    case TypeKind::FNDEF:
      return "FnDef";

    case TypeKind::FNPTR:
      return "FnPtr";

    case TypeKind::TUPLE:
      return "Tuple";

    case TypeKind::BOOL:
      return "Bool";

    case TypeKind::CHAR:
      return "Char";

    case TypeKind::INT:
      return "Int";

    case TypeKind::UINT:
      return "Uint";

    case TypeKind::FLOAT:
      return "Float";

    case TypeKind::USIZE:
      return "Usize";

    case TypeKind::ISIZE:
      return "Isize";

    case TypeKind::NEVER:
      return "Never";

    case TypeKind::PLACEHOLDER:
      return "Placeholder";

    case TypeKind::PROJECTION:
      return "Projection";

    case TypeKind::DYNAMIC:
      return "Dynamic";

    case TypeKind::CLOSURE:
      return "Closure";

    case TypeKind::OPAQUE:
      return "Opaque";

    case TypeKind::ERROR:
      return "ERROR";
    }
  rust_unreachable ();
}

bool
is_primitive_type_kind (TypeKind kind)
{
  switch (kind)
    {
    case TypeKind::BOOL:
    case TypeKind::CHAR:
    case TypeKind::INT:
    case TypeKind::UINT:
    case TypeKind::ISIZE:
    case TypeKind::USIZE:
    case TypeKind::FLOAT:
    case TypeKind::NEVER:
    case TypeKind::STR:
      return true;
    default:
      return false;
    }
}

// BASE TYPE

BaseType::BaseType (HirId ref, HirId ty_ref, TypeKind kind, RustIdent ident,
		    std::set<HirId> refs)
  : TypeBoundsMappings ({}), kind (kind), ref (ref), ty_ref (ty_ref),
    orig_ref (ref), combined (refs), ident (ident),
    mappings (Analysis::Mappings::get ())
{}

BaseType::BaseType (HirId ref, HirId ty_ref, TypeKind kind, RustIdent ident,
		    std::vector<TypeBoundPredicate> specified_bounds,
		    std::set<HirId> refs)
  : TypeBoundsMappings (specified_bounds), kind (kind), ref (ref),
    ty_ref (ty_ref), orig_ref (ref), combined (refs), ident (ident),
    mappings (Analysis::Mappings::get ())
{}

BaseType::~BaseType () {}

HirId
BaseType::get_ref () const
{
  return ref;
}

void
BaseType::set_ref (HirId id)
{
  if (id != ref)
    append_reference (ref);
  ref = id;
}

HirId
BaseType::get_ty_ref () const
{
  return ty_ref;
}

void
BaseType::set_ty_ref (HirId id)
{
  ty_ref = id;
}
HirId
BaseType::get_orig_ref () const
{
  return orig_ref;
}

bool
BaseType::is_equal (const BaseType &other) const
{
  return get_kind () == other.get_kind ();
}

bool
BaseType::is_unit () const
{
  const TyTy::BaseType *x = destructure ();
  switch (x->get_kind ())
    {
    case PARAM:
    case PROJECTION:
    case PLACEHOLDER:
    case FNPTR:
    case FNDEF:
    case ARRAY:
    case SLICE:
    case POINTER:
    case REF:
    case CLOSURE:
    case INFER:
    case BOOL:
    case CHAR:
    case INT:
    case UINT:
    case FLOAT:
    case USIZE:
    case ISIZE:
    case OPAQUE:
    case STR:
    case DYNAMIC:
    case ERROR:
      return false;

      // FIXME ! is coerceable to () so we need to fix that
    case NEVER:
      return true;

      case TUPLE: {
	return x->as<const TupleType> ()->num_fields () == 0;
      }

      case ADT: {
	auto adt = x->as<const ADTType> ();
	if (adt->is_enum ())
	  return false;

	for (const auto &variant : adt->get_variants ())
	  {
	    if (variant->num_fields () > 0)
	      return false;
	  }

	return true;
      }
    }
  return false;
}

TypeKind
BaseType::get_kind () const
{
  return kind;
}

std::set<HirId>
BaseType::get_combined_refs () const
{
  return combined;
}

void
BaseType::append_reference (HirId id)
{
  combined.insert (id);
}

const RustIdent &
BaseType::get_ident () const
{
  return ident;
}

location_t
BaseType::get_locus () const
{
  return ident.locus;
}

// FIXME this is missing locus
bool
BaseType::satisfies_bound (const TypeBoundPredicate &predicate,
			   bool emit_error) const
{
  const Resolver::TraitReference *query = predicate.get ();
  for (const auto &bound : specified_bounds)
    {
      const Resolver::TraitReference *item = bound.get ();
      if (item->satisfies_bound (*query))
	return true;
    }

  if (destructure ()->is<InferType> ())
    return true;

  bool satisfied = false;
  auto probed = Resolver::TypeBoundsProbe::Probe (this);
  for (const auto &b : probed)
    {
      const Resolver::TraitReference *bound = b.first;
      if (bound->satisfies_bound (*query))
	{
	  satisfied = true;
	  break;
	}
    }

  if (!satisfied)
    return false;

  for (const auto &b : probed)
    {
      const Resolver::TraitReference *bound = b.first;
      if (!bound->is_equal (*query))
	continue;

      // builtin ones have no impl-block this needs fixed and use a builtin node
      // of somekind
      if (b.second == nullptr)
	return true;

      // need to check that associated types can match as well
      const HIR::ImplBlock &impl = *(b.second);
      for (const auto &item : impl.get_impl_items ())
	{
	  bool is_associated_type = item->get_impl_item_type ()
				    == HIR::ImplItem::ImplItemType::TYPE_ALIAS;
	  if (!is_associated_type)
	    continue;

	  TyTy::BaseType *impl_item_ty = nullptr;
	  Analysis::NodeMapping i = item->get_impl_mappings ();
	  bool query_ok = Resolver::query_type (i.get_hirid (), &impl_item_ty);
	  if (!query_ok)
	    return false;

	  std::string item_name = item->get_impl_item_name ();
	  TypeBoundPredicateItem lookup
	    = predicate.lookup_associated_item (item_name);
	  if (lookup.is_error ())
	    return false;

	  const auto *item_ref = lookup.get_raw_item ();
	  TyTy::BaseType *bound_ty = item_ref->get_tyty ();

	  // compare the types
	  if (!bound_ty->can_eq (impl_item_ty, false))
	    {
	      if (!impl_item_ty->can_eq (bound_ty, false))
		{
		  if (emit_error)
		    {
		      rich_location r (line_table,
				       mappings.lookup_location (get_ref ()));
		      r.add_range (predicate.get_locus ());
		      r.add_range (mappings.lookup_location (i.get_hirid ()));

		      std::string rich_msg
			= "expected " + bound_ty->destructure ()->get_name ()
			  + ", found "
			  + impl_item_ty->destructure ()->get_name ();
		      r.add_fixit_replace (rich_msg.c_str ());

		      rust_error_at (
			r, ErrorCode::E0271,
			"type mismatch, expected %qs but got %qs",
			bound_ty->destructure ()->get_name ().c_str (),
			impl_item_ty->destructure ()->get_name ().c_str ());
		    }
		  return false;
		}
	    }
	}

      return true;
    }

  return false;
}

bool
BaseType::bounds_compatible (const BaseType &other, location_t locus,
			     bool emit_error) const
{
  std::vector<std::reference_wrapper<const TypeBoundPredicate>>
    unsatisfied_bounds;
  for (auto &bound : get_specified_bounds ())
    {
      if (!other.satisfies_bound (bound, emit_error))
	unsatisfied_bounds.push_back (bound);
    }

  // lets emit a single error for this
  if (unsatisfied_bounds.size () > 0)
    {
      rich_location r (line_table, locus);
      std::string missing_preds;
      for (size_t i = 0; i < unsatisfied_bounds.size (); i++)
	{
	  const TypeBoundPredicate &pred = unsatisfied_bounds.at (i);
	  r.add_range (pred.get_locus ());
	  missing_preds += pred.get_name ();

	  bool have_next = (i + 1) < unsatisfied_bounds.size ();
	  if (have_next)
	    missing_preds += ", ";
	}

      if (emit_error)
	{
	  rust_error_at (r, ErrorCode::E0277,
			 "bounds not satisfied for %s %qs is not satisfied",
			 other.get_name ().c_str (), missing_preds.c_str ());
	  // rust_assert (!emit_error);
	}
    }

  return unsatisfied_bounds.size () == 0;
}

void
BaseType::inherit_bounds (const BaseType &other)
{
  inherit_bounds (other.get_specified_bounds ());
}

void
BaseType::inherit_bounds (
  const std::vector<TyTy::TypeBoundPredicate> &specified_bounds)
{
  for (auto &bound : specified_bounds)
    {
      add_bound (bound);
    }
}

const BaseType *
BaseType::get_root () const
{
  // FIXME this needs to be it its own visitor class with a vector adjustments
  const TyTy::BaseType *root = this;

  if (const auto r = root->try_as<const ReferenceType> ())
    {
      root = r->get_base ()->get_root ();
    }
  else if (const auto r = root->try_as<const PointerType> ())
    {
      root = r->get_base ()->get_root ();
    }
  // these are an unsize
  else if (const auto r = root->try_as<const SliceType> ())
    {
      root = r->get_element_type ()->get_root ();
    }
  //  else if (const auto r = root->try_as<const ArrayType> ())
  //    {
  //      root = r->get_element_type ()->get_root ();
  //    }

  return root;
}

BaseType *
BaseType::destructure ()
{
  int recurisve_ops = 0;
  BaseType *x = this;
  while (true)
    {
      if (recurisve_ops++ >= rust_max_recursion_depth)
	{
	  rust_error_at (
	    UNDEF_LOCATION,
	    "%<recursion depth%> count exceeds limit of %i (use "
	    "%<frust-max-recursion-depth=%> to increase the limit)",
	    rust_max_recursion_depth);
	  return new ErrorType (get_ref ());
	}

      if (auto p = x->try_as<ParamType> ())
	{
	  auto pr = p->resolve ();
	  if (pr == x)
	    return pr;

	  x = pr;
	}
      else if (auto p = x->try_as<PlaceholderType> ())
	{
	  if (!p->can_resolve ())
	    return p;

	  x = p->resolve ();
	}
      else if (auto p = x->try_as<ProjectionType> ())
	{
	  x = p->get ();
	}
      else
	{
	  return x;
	}
    }

  return x;
}

const BaseType *
BaseType::destructure () const
{
  int recurisve_ops = 0;
  const BaseType *x = this;
  while (true)
    {
      if (recurisve_ops++ >= rust_max_recursion_depth)
	{
	  rust_error_at (
	    UNDEF_LOCATION,
	    "%<recursion depth%> count exceeds limit of %i (use "
	    "%<frust-max-recursion-depth=%> to increase the limit)",
	    rust_max_recursion_depth);
	  return new ErrorType (get_ref ());
	}

      if (auto p = x->try_as<const ParamType> ())
	{
	  auto pr = p->resolve ();
	  if (pr == x)
	    return pr;

	  x = pr;
	}
      else if (auto p = x->try_as<const PlaceholderType> ())
	{
	  if (!p->can_resolve ())
	    return p;

	  x = p->resolve ();
	}
      else if (auto p = x->try_as<const ProjectionType> ())
	{
	  x = p->get ();
	}
      // else if (auto p = x->try_as<const OpaqueType> ())
      //   {
      //     auto pr = p->resolve ();

      //     rust_debug ("XXXXXX")

      //     if (pr == x)
      //       return pr;

      //     x = pr;
      //   }
      else
	{
	  return x;
	}
    }

  return x;
}

BaseType *
BaseType::monomorphized_clone () const
{
  const TyTy::BaseType *x = destructure ();

  if (auto arr = x->try_as<const ArrayType> ())
    {
      TyVar elm = arr->get_var_element_type ().monomorphized_clone ();
      return new ArrayType (arr->get_ref (), arr->get_ty_ref (), ident.locus,
			    arr->get_capacity_expr (), elm,
			    arr->get_combined_refs ());
    }
  else if (auto slice = x->try_as<const SliceType> ())
    {
      TyVar elm = slice->get_var_element_type ().monomorphized_clone ();
      return new SliceType (slice->get_ref (), slice->get_ty_ref (),
			    ident.locus, elm, slice->get_combined_refs ());
    }
  else if (auto ptr = x->try_as<const PointerType> ())
    {
      TyVar elm = ptr->get_var_element_type ().monomorphized_clone ();
      return new PointerType (ptr->get_ref (), ptr->get_ty_ref (), elm,
			      ptr->mutability (), ptr->get_combined_refs ());
    }
  else if (auto ref = x->try_as<const ReferenceType> ())
    {
      TyVar elm = ref->get_var_element_type ().monomorphized_clone ();
      return new ReferenceType (ref->get_ref (), ref->get_ty_ref (), elm,
				ref->mutability (), ref->get_region (),
				ref->get_combined_refs ());
    }
  else if (auto tuple = x->try_as<const TupleType> ())
    {
      std::vector<TyVar> cloned_fields;
      for (const auto &f : tuple->get_fields ())
	cloned_fields.push_back (f.monomorphized_clone ());

      return new TupleType (tuple->get_ref (), tuple->get_ty_ref (),
			    ident.locus, cloned_fields,
			    tuple->get_combined_refs ());
    }
  else if (auto fn = x->try_as<const FnType> ())
    {
      std::vector<TyTy::FnParam> cloned_params;
      for (auto &p : fn->get_params ())
	cloned_params.push_back (p.monomorphized_clone ());

      BaseType *retty = fn->get_return_type ()->monomorphized_clone ();
      return new FnType (fn->get_ref (), fn->get_ty_ref (), fn->get_id (),
			 fn->get_identifier (), fn->ident, fn->get_flags (),
			 fn->get_abi (), std::move (cloned_params), retty,
			 fn->clone_substs (), fn->get_substitution_arguments (),
			 fn->get_region_constraints (),
			 fn->get_combined_refs ());
    }
  else if (auto fn = x->try_as<const FnPtr> ())
    {
      std::vector<TyVar> cloned_params;
      for (auto &p : fn->get_params ())
	cloned_params.push_back (p.monomorphized_clone ());

      TyVar retty = fn->get_var_return_type ().monomorphized_clone ();
      return new FnPtr (fn->get_ref (), fn->get_ty_ref (), ident.locus,
			std::move (cloned_params), retty,
			fn->get_combined_refs ());
    }
  else if (auto adt = x->try_as<const ADTType> ())
    {
      std::vector<VariantDef *> cloned_variants;
      for (auto &variant : adt->get_variants ())
	cloned_variants.push_back (variant->monomorphized_clone ());

      return new ADTType (adt->get_id (), adt->get_ref (), adt->get_ty_ref (),
			  adt->get_identifier (), adt->ident,
			  adt->get_adt_kind (), cloned_variants,
			  adt->clone_substs (), adt->get_repr_options (),
			  adt->get_used_arguments (),
			  adt->get_region_constraints (),
			  adt->get_combined_refs ());
    }
  else
    {
      return x->clone ();
    }

  rust_unreachable ();
  return nullptr;
}

std::string
BaseType::mappings_str () const
{
  std::string buffer = "Ref: " + std::to_string (get_ref ())
		       + " TyRef: " + std::to_string (get_ty_ref ());
  buffer += "[";
  for (auto &ref : combined)
    buffer += std::to_string (ref) + ",";
  buffer += "]";
  return "(" + buffer + ")";
}

std::string
BaseType::debug_str () const
{
  // return TypeKindFormat::to_string (get_kind ()) + ":" + as_string () + ":"
  //        + mappings_str () + ":" + bounds_as_string ();
  return get_name ();
}

void
BaseType::debug () const
{
  rust_debug ("[%p] %s", static_cast<const void *> (this),
	      debug_str ().c_str ());
}

bool
BaseType::is_concrete () const
{
  const TyTy::BaseType *x = destructure ();

  if (x->is<ParamType> () || x->is<ProjectionType> ())
    {
      return false;
    }
  // placeholder is a special case for this case when it is not resolvable
  // it means we its just an empty placeholder associated type which is
  // concrete
  else if (x->is<PlaceholderType> ())
    {
      return true;
    }
  else if (auto fn = x->try_as<const FnType> ())
    {
      for (const auto &param : fn->get_params ())
	{
	  if (!param.get_type ()->is_concrete ())
	    return false;
	}
      return fn->get_return_type ()->is_concrete ();
    }
  else if (auto fn = x->try_as<const FnPtr> ())
    {
      for (const auto &param : fn->get_params ())
	{
	  if (!param.get_tyty ()->is_concrete ())
	    return false;
	}
      return fn->get_return_type ()->is_concrete ();
    }
  else if (auto adt = x->try_as<const ADTType> ())
    {
      if (adt->is_unit ())
	return !adt->needs_substitution ();

      for (auto &variant : adt->get_variants ())
	{
	  bool is_num_variant
	    = variant->get_variant_type () == VariantDef::VariantType::NUM;
	  if (is_num_variant)
	    continue;

	  for (auto &field : variant->get_fields ())
	    {
	      const BaseType *field_type = field->get_field_type ();
	      if (!field_type->is_concrete ())
		return false;
	    }
	}
      return true;
    }
  else if (auto arr = x->try_as<const ArrayType> ())
    {
      return arr->get_element_type ()->is_concrete ();
    }
  else if (auto slice = x->try_as<const SliceType> ())
    {
      return slice->get_element_type ()->is_concrete ();
    }
  else if (auto ptr = x->try_as<const PointerType> ())
    {
      return ptr->get_base ()->is_concrete ();
    }
  else if (auto ref = x->try_as<const ReferenceType> ())
    {
      return ref->get_base ()->is_concrete ();
    }
  else if (auto tuple = x->try_as<const TupleType> ())
    {
      for (size_t i = 0; i < tuple->num_fields (); i++)
	{
	  if (!tuple->get_field (i)->is_concrete ())
	    return false;
	}
      return true;
    }
  else if (auto closure = x->try_as<const ClosureType> ())
    {
      if (closure->get_parameters ().is_concrete ())
	return false;
      return closure->get_result_type ().is_concrete ();
    }
  else if (x->is<InferType> () || x->is<BoolType> () || x->is<CharType> ()
	   || x->is<IntType> () || x->is<UintType> () || x->is<FloatType> ()
	   || x->is<USizeType> () || x->is<ISizeType> () || x->is<NeverType> ()
	   || x->is<StrType> () || x->is<DynamicObjectType> ()
	   || x->is<ErrorType> ())
    {
      return true;
    }

  return false;
}

bool
BaseType::has_substitutions_defined () const
{
  const TyTy::BaseType *x = destructure ();
  switch (x->get_kind ())
    {
    case INFER:
    case BOOL:
    case CHAR:
    case INT:
    case UINT:
    case FLOAT:
    case USIZE:
    case ISIZE:
    case NEVER:
    case STR:
    case DYNAMIC:
    case ERROR:
    case FNPTR:
    case ARRAY:
    case SLICE:
    case POINTER:
    case REF:
    case TUPLE:
    case PARAM:
    case PLACEHOLDER:
    case OPAQUE:
      return false;

      case PROJECTION: {
	const ProjectionType &p = *static_cast<const ProjectionType *> (x);
	const SubstitutionRef &ref = static_cast<const SubstitutionRef &> (p);
	return ref.has_substitutions ();
      }
      break;

      case FNDEF: {
	const FnType &fn = *static_cast<const FnType *> (x);
	const SubstitutionRef &ref = static_cast<const SubstitutionRef &> (fn);
	return ref.has_substitutions ();
      }
      break;

      case ADT: {
	const ADTType &adt = *static_cast<const ADTType *> (x);
	const SubstitutionRef &ref = static_cast<const SubstitutionRef &> (adt);
	return ref.has_substitutions ();
      }
      break;

      case CLOSURE: {
	const ClosureType &closure = *static_cast<const ClosureType *> (x);
	const SubstitutionRef &ref
	  = static_cast<const SubstitutionRef &> (closure);
	return ref.has_substitutions ();
      }
      break;
    }

  return false;
}

bool
BaseType::needs_generic_substitutions () const
{
  const TyTy::BaseType *x = destructure ();
  switch (x->get_kind ())
    {
    case INFER:
    case BOOL:
    case CHAR:
    case INT:
    case UINT:
    case FLOAT:
    case USIZE:
    case ISIZE:
    case NEVER:
    case STR:
    case DYNAMIC:
    case ERROR:
    case FNPTR:
    case ARRAY:
    case SLICE:
    case POINTER:
    case REF:
    case TUPLE:
    case PARAM:
    case PLACEHOLDER:
    case OPAQUE:
      return false;

      case PROJECTION: {
	const ProjectionType &p = *static_cast<const ProjectionType *> (x);
	const SubstitutionRef &ref = static_cast<const SubstitutionRef &> (p);
	return ref.needs_substitution ();
      }
      break;

      case FNDEF: {
	const FnType &fn = *static_cast<const FnType *> (x);
	const SubstitutionRef &ref = static_cast<const SubstitutionRef &> (fn);
	return ref.needs_substitution ();
      }
      break;

      case ADT: {
	const ADTType &adt = *static_cast<const ADTType *> (x);
	const SubstitutionRef &ref = static_cast<const SubstitutionRef &> (adt);
	return ref.needs_substitution ();
      }
      break;

      case CLOSURE: {
	const ClosureType &closure = *static_cast<const ClosureType *> (x);
	const SubstitutionRef &ref
	  = static_cast<const SubstitutionRef &> (closure);
	return ref.needs_substitution ();
      }
      break;
    }

  return false;
}

const SubstitutionArgumentMappings &
BaseType::get_subst_argument_mappings () const
{
  static auto empty = SubstitutionArgumentMappings::empty ();
  const TyTy::BaseType *x = destructure ();
  switch (x->get_kind ())
    {
      case PROJECTION: {
	const auto &p = *static_cast<const ProjectionType *> (x);
	const auto &ref = static_cast<const SubstitutionRef &> (p);
	return ref.get_substitution_arguments ();
      }
      break;

      case FNDEF: {
	const auto &fn = *static_cast<const FnType *> (x);
	const auto &ref = static_cast<const SubstitutionRef &> (fn);
	return ref.get_substitution_arguments ();
      }
      break;

      case ADT: {
	const auto &adt = *static_cast<const ADTType *> (x);
	const auto &ref = static_cast<const SubstitutionRef &> (adt);
	return ref.get_substitution_arguments ();
      }
      break;

      case CLOSURE: {
	const auto &closure = *static_cast<const ClosureType *> (x);
	const auto &ref = static_cast<const SubstitutionRef &> (closure);
	return ref.get_substitution_arguments ();
      }
      break;

    default:
      return empty;
    }

  return empty;
}

// InferType

InferType::InferType (HirId ref, InferTypeKind infer_kind, TypeHint hint,
		      location_t locus, std::set<HirId> refs)
  : BaseType (ref, ref, KIND, {Resolver::CanonicalPath::create_empty (), locus},
	      refs),
    infer_kind (infer_kind), default_hint (hint)
{}

InferType::InferType (HirId ref, HirId ty_ref, InferTypeKind infer_kind,
		      TypeHint hint, location_t locus, std::set<HirId> refs)
  : BaseType (ref, ty_ref, KIND,
	      {Resolver::CanonicalPath::create_empty (), locus}, refs),
    infer_kind (infer_kind), default_hint (hint)
{}

InferType::InferTypeKind
InferType::get_infer_kind () const
{
  return infer_kind;
}

std::string
InferType::get_name () const
{
  return as_string ();
}

void
InferType::accept_vis (TyVisitor &vis)
{
  vis.visit (*this);
}

void
InferType::accept_vis (TyConstVisitor &vis) const
{
  vis.visit (*this);
}

std::string
InferType::as_string () const
{
  switch (infer_kind)
    {
    case GENERAL:
      return "T?";
    case INTEGRAL:
      return "<integer>";
    case FLOAT:
      return "<float>";
    }
  return "<infer::error>";
}

bool
InferType::can_eq (const BaseType *other, bool emit_errors) const
{
  InferCmp r (this, emit_errors);
  return r.can_eq (other);
}

BaseType *
InferType::clone () const
{
  // clones for inference variables are special in that they _must_ exist within
  // the type check context and we must ensure we don't loose the chain
  // otherwise we will end up in the missing type annotations case
  //
  // This means we cannot simply take over the same reference we must generate a
  // new ref just like the get_implicit_infer_var code then we can setup the
  // chain of references accordingly to ensure we don't loose the ability to
  // update the inference variables when we solve the type

  auto &mappings = Analysis::Mappings::get ();
  auto context = Resolver::TypeCheckContext::get ();

  InferType *clone
    = new InferType (mappings.get_next_hir_id (), get_infer_kind (),
		     default_hint, get_ident ().locus, get_combined_refs ());

  context->insert_type (Analysis::NodeMapping (mappings.get_current_crate (),
					       UNKNOWN_NODEID,
					       clone->get_ref (),
					       UNKNOWN_LOCAL_DEFID),
			clone);
  mappings.insert_location (clone->get_ref (),
			    mappings.lookup_location (get_ref ()));

  // setup the chain to reference this
  clone->append_reference (get_ref ());

  return clone;
}

bool
InferType::default_type (BaseType **type) const
{
  auto context = Resolver::TypeCheckContext::get ();
  bool ok = false;

  // NOTE: Calling this error is misleading.
  if (default_hint.kind == TypeKind::ERROR)
    {
      switch (infer_kind)
	{
	case GENERAL:
	  return false;

	  case INTEGRAL: {
	    ok = context->lookup_builtin ("i32", type);
	    rust_assert (ok);
	    return ok;
	  }

	  case FLOAT: {
	    ok = context->lookup_builtin ("f64", type);
	    rust_assert (ok);
	    return ok;
	  }
	}
      return false;
    }

  switch (default_hint.kind)
    {
    case ISIZE:
      ok = context->lookup_builtin ("isize", type);
      rust_assert (ok);
      return ok;

    case USIZE:
      ok = context->lookup_builtin ("usize", type);
      rust_assert (ok);
      return ok;

    case INT:
      switch (default_hint.szhint)
	{
	case TypeHint::SizeHint::S8:
	  ok = context->lookup_builtin ("i8", type);
	  rust_assert (ok);
	  return ok;

	case TypeHint::SizeHint::S16:
	  ok = context->lookup_builtin ("i16", type);
	  rust_assert (ok);
	  return ok;

	case TypeHint::SizeHint::S32:
	  ok = context->lookup_builtin ("i32", type);
	  rust_assert (ok);
	  return ok;

	case TypeHint::SizeHint::S64:
	  ok = context->lookup_builtin ("i64", type);
	  rust_assert (ok);
	  return ok;

	case TypeHint::SizeHint::S128:
	  ok = context->lookup_builtin ("i128", type);
	  rust_assert (ok);
	  return ok;

	default:
	  return false;
	}
      break;

    case UINT:
      switch (default_hint.szhint)
	{
	case TypeHint::SizeHint::S8:
	  ok = context->lookup_builtin ("u8", type);
	  rust_assert (ok);
	  return ok;

	case TypeHint::SizeHint::S16:
	  ok = context->lookup_builtin ("u16", type);
	  rust_assert (ok);
	  return ok;

	case TypeHint::SizeHint::S32:
	  ok = context->lookup_builtin ("u32", type);
	  rust_assert (ok);
	  return ok;

	case TypeHint::SizeHint::S64:
	  ok = context->lookup_builtin ("u64", type);
	  rust_assert (ok);
	  return ok;

	case TypeHint::SizeHint::S128:
	  ok = context->lookup_builtin ("u128", type);
	  rust_assert (ok);
	  return ok;

	default:
	  return false;
	}
      break;

    case TypeKind::FLOAT:
      switch (default_hint.szhint)
	{
	case TypeHint::SizeHint::S32:
	  ok = context->lookup_builtin ("f32", type);
	  rust_assert (ok);
	  return ok;

	case TypeHint::SizeHint::S64:
	  ok = context->lookup_builtin ("f64", type);
	  rust_assert (ok);
	  return ok;

	default:
	  return false;
	}
      break;

    default:
      return false;
    }

  return false;
}

void
InferType::apply_primitive_type_hint (const BaseType &hint)
{
  switch (hint.get_kind ())
    {
    case ISIZE:
    case USIZE:
      infer_kind = INTEGRAL;
      default_hint.kind = hint.get_kind ();
      break;

      case INT: {
	infer_kind = INTEGRAL;
	default_hint.kind = hint.get_kind ();
	default_hint.shint = TypeHint::SignedHint::SIGNED;
	switch (hint.as<const IntType> ()->get_int_kind ())
	  {
	  case IntType::I8:
	    default_hint.szhint = TypeHint::SizeHint::S8;
	    break;
	  case IntType::I16:
	    default_hint.szhint = TypeHint::SizeHint::S16;
	    break;
	  case IntType::I32:
	    default_hint.szhint = TypeHint::SizeHint::S32;
	    break;
	  case IntType::I64:
	    default_hint.szhint = TypeHint::SizeHint::S64;
	    break;
	  case IntType::I128:
	    default_hint.szhint = TypeHint::SizeHint::S128;
	    break;
	  }
      }
      break;

      case UINT: {
	infer_kind = INTEGRAL;
	default_hint.kind = hint.get_kind ();
	default_hint.shint = TypeHint::SignedHint::UNSIGNED;
	switch (hint.as<const UintType> ()->get_uint_kind ())
	  {
	  case UintType::U8:
	    default_hint.szhint = TypeHint::SizeHint::S8;
	    break;
	  case UintType::U16:
	    default_hint.szhint = TypeHint::SizeHint::S16;
	    break;
	  case UintType::U32:
	    default_hint.szhint = TypeHint::SizeHint::S32;
	    break;
	  case UintType::U64:
	    default_hint.szhint = TypeHint::SizeHint::S64;
	    break;
	  case UintType::U128:
	    default_hint.szhint = TypeHint::SizeHint::S128;
	    break;
	  }
      }
      break;

      case TypeKind::FLOAT: {
	infer_kind = FLOAT;
	default_hint.shint = TypeHint::SignedHint::SIGNED;
	default_hint.kind = hint.get_kind ();
	switch (hint.as<const FloatType> ()->get_float_kind ())
	  {
	  case FloatType::F32:
	    default_hint.szhint = TypeHint::SizeHint::S32;
	    break;

	  case FloatType::F64:
	    default_hint.szhint = TypeHint::SizeHint::S64;
	    break;
	  }
      }
      break;

    default:
      // TODO bool, char, never??
      break;
    }
}

// ErrorType

ErrorType::ErrorType (HirId ref, std::set<HirId> refs)
  : BaseType (ref, ref, KIND,
	      {Resolver::CanonicalPath::create_empty (), UNDEF_LOCATION}, refs)
{}

ErrorType::ErrorType (HirId ref, HirId ty_ref, std::set<HirId> refs)
  : BaseType (ref, ty_ref, KIND,
	      {Resolver::CanonicalPath::create_empty (), UNDEF_LOCATION}, refs)
{}

std::string
ErrorType::get_name () const
{
  return as_string ();
}

void
ErrorType::accept_vis (TyVisitor &vis)
{
  vis.visit (*this);
}

void
ErrorType::accept_vis (TyConstVisitor &vis) const
{
  vis.visit (*this);
}

std::string
ErrorType::as_string () const
{
  return "<tyty::error>";
}

bool
ErrorType::can_eq (const BaseType *other, bool emit_errors) const
{
  return get_kind () == other->get_kind ();
}

BaseType *
ErrorType::clone () const
{
  return new ErrorType (get_ref (), get_ty_ref (), get_combined_refs ());
}

// Struct Field type

StructFieldType::StructFieldType (HirId ref, std::string name, BaseType *ty,
				  location_t locus)
  : ref (ref), name (name), ty (ty), locus (locus)
{}

HirId
StructFieldType::get_ref () const
{
  return ref;
}

std::string
StructFieldType::get_name () const
{
  return name;
}

BaseType *
StructFieldType::get_field_type () const
{
  return ty;
}

void
StructFieldType::set_field_type (BaseType *fty)
{
  ty = fty;
}

void
StructFieldType::debug () const
{
  rust_debug ("%s", as_string ().c_str ());
}

location_t
StructFieldType::get_locus () const
{
  return locus;
}

std::string
StructFieldType::as_string () const
{
  return name + ":" + get_field_type ()->debug_str ();
}

bool
StructFieldType::is_equal (const StructFieldType &other) const
{
  bool names_eq = get_name () == other.get_name ();

  TyTy::BaseType *o = other.get_field_type ();
  if (auto op = o->try_as<ParamType> ())
    o = op->resolve ();

  bool types_eq = get_field_type ()->is_equal (*o);

  return names_eq && types_eq;
}

StructFieldType *
StructFieldType::clone () const
{
  return new StructFieldType (get_ref (), get_name (),
			      get_field_type ()->clone (), locus);
}

StructFieldType *
StructFieldType::monomorphized_clone () const
{
  return new StructFieldType (get_ref (), get_name (),
			      get_field_type ()->monomorphized_clone (), locus);
}

// VariantDef

std::string
VariantDef::variant_type_string (VariantType type)
{
  switch (type)
    {
    case NUM:
      return "enumeral";
    case TUPLE:
      return "tuple";
    case STRUCT:
      return "struct";
    }
  rust_unreachable ();
  return "";
}

VariantDef::VariantDef (HirId id, DefId defid, std::string identifier,
			RustIdent ident,
			tl::optional<std::unique_ptr<HIR::Expr>> &&discriminant)
  : id (id), defid (defid), identifier (identifier), ident (ident),
    discriminant (std::move (discriminant))

{
  type = VariantType::NUM;
  fields = {};
}

VariantDef::VariantDef (HirId id, DefId defid, std::string identifier,
			RustIdent ident, VariantType type,
			tl::optional<std::unique_ptr<HIR::Expr>> &&discriminant,
			std::vector<StructFieldType *> fields)
  : id (id), defid (defid), identifier (identifier), ident (ident), type (type),
    discriminant (std::move (discriminant)), fields (fields)
{
  rust_assert ((type == VariantType::NUM && fields.empty ())
	       || (type == VariantType::TUPLE || type == VariantType::STRUCT));
}

VariantDef &
VariantDef::get_error_node ()
{
  static VariantDef node
    = VariantDef (UNKNOWN_HIRID, UNKNOWN_DEFID, "",
		  {Resolver::CanonicalPath::create_empty (), UNKNOWN_LOCATION},
		  tl::nullopt);

  return node;
}

bool
VariantDef::is_error () const
{
  return get_id () == UNKNOWN_HIRID;
}

HirId
VariantDef::get_id () const
{
  return id;
}

DefId
VariantDef::get_defid () const
{
  return defid;
}

VariantDef::VariantType
VariantDef::get_variant_type () const
{
  return type;
}

bool
VariantDef::is_data_variant () const
{
  return type != VariantType::NUM;
}

bool
VariantDef::is_dataless_variant () const
{
  return type == VariantType::NUM;
}

std::string
VariantDef::get_identifier () const
{
  return identifier;
}

size_t
VariantDef::num_fields () const
{
  return fields.size ();
}

StructFieldType *
VariantDef::get_field_at_index (size_t index)
{
  rust_assert (index < fields.size ());
  return fields.at (index);
}

std::vector<StructFieldType *> &
VariantDef::get_fields ()
{
  rust_assert (type != NUM);
  return fields;
}

bool
VariantDef::lookup_field (const std::string &lookup,
			  StructFieldType **field_lookup, size_t *index) const
{
  size_t i = 0;
  for (auto &field : fields)
    {
      if (field->get_name ().compare (lookup) == 0)
	{
	  if (index != nullptr)
	    *index = i;

	  if (field_lookup != nullptr)
	    *field_lookup = field;

	  return true;
	}
      i++;
    }
  return false;
}

HIR::Expr &
VariantDef::get_discriminant ()
{
  return *discriminant.value ();
}

const HIR::Expr &
VariantDef::get_discriminant () const
{
  return *discriminant.value ();
}

bool
VariantDef::has_discriminant () const
{
  return discriminant.has_value ();
}

std::string
VariantDef::as_string () const
{
  if (type == VariantType::NUM)
    return identifier
	   + (has_discriminant () ? " = " + get_discriminant ().as_string ()
				  : "");

  std::string buffer;
  for (size_t i = 0; i < fields.size (); ++i)
    {
      buffer += fields.at (i)->as_string ();
      if ((i + 1) < fields.size ())
	buffer += ", ";
    }

  if (type == VariantType::TUPLE)
    return identifier + " (" + buffer + ")";
  else
    return identifier + " {" + buffer + "}";
}

bool
VariantDef::is_equal (const VariantDef &other) const
{
  if (type != other.type)
    return false;

  if (identifier.compare (other.identifier) != 0)
    return false;

  if (fields.size () != other.fields.size ())
    return false;

  for (size_t i = 0; i < fields.size (); i++)
    {
      if (!fields.at (i)->is_equal (*other.fields.at (i)))
	return false;
    }

  return true;
}

VariantDef *
VariantDef::clone () const
{
  std::vector<StructFieldType *> cloned_fields;
  for (auto &f : fields)
    cloned_fields.push_back ((StructFieldType *) f->clone ());

  auto &&discriminant_opt = has_discriminant ()
			      ? tl::optional<std::unique_ptr<HIR::Expr>> (
				get_discriminant ().clone_expr ())
			      : tl::nullopt;

  return new VariantDef (id, defid, identifier, ident, type,
			 std::move (discriminant_opt), cloned_fields);
}

VariantDef *
VariantDef::monomorphized_clone () const
{
  std::vector<StructFieldType *> cloned_fields;
  for (auto &f : fields)
    cloned_fields.push_back ((StructFieldType *) f->monomorphized_clone ());

  auto discriminant_opt = has_discriminant ()
			    ? tl::optional<std::unique_ptr<HIR::Expr>> (
			      get_discriminant ().clone_expr ())
			    : tl::nullopt;

  return new VariantDef (id, defid, identifier, ident, type,
			 std::move (discriminant_opt), cloned_fields);
}

const RustIdent &
VariantDef::get_ident () const
{
  return ident;
}

// ADTType

ADTType::ADTType (DefId id, HirId ref, std::string identifier, RustIdent ident,
		  ADTKind adt_kind, std::vector<VariantDef *> variants,
		  std::vector<SubstitutionParamMapping> subst_refs,
		  SubstitutionArgumentMappings generic_arguments,
		  RegionConstraints region_constraints, std::set<HirId> refs)
  : BaseType (ref, ref, TypeKind::ADT, ident, refs),
    SubstitutionRef (std::move (subst_refs), std::move (generic_arguments),
		     region_constraints),
    id (id), identifier (identifier), variants (variants), adt_kind (adt_kind)
{}

ADTType::ADTType (DefId id, HirId ref, HirId ty_ref, std::string identifier,
		  RustIdent ident, ADTKind adt_kind,
		  std::vector<VariantDef *> variants,
		  std::vector<SubstitutionParamMapping> subst_refs,
		  SubstitutionArgumentMappings generic_arguments,
		  RegionConstraints region_constraints, std::set<HirId> refs)
  : BaseType (ref, ty_ref, TypeKind::ADT, ident, refs),
    SubstitutionRef (std::move (subst_refs), std::move (generic_arguments),
		     region_constraints),
    id (id), identifier (identifier), variants (variants), adt_kind (adt_kind)
{}

ADTType::ADTType (DefId id, HirId ref, HirId ty_ref, std::string identifier,
		  RustIdent ident, ADTKind adt_kind,
		  std::vector<VariantDef *> variants,
		  std::vector<SubstitutionParamMapping> subst_refs,
		  ReprOptions repr,
		  SubstitutionArgumentMappings generic_arguments,
		  RegionConstraints region_constraints, std::set<HirId> refs)
  : BaseType (ref, ty_ref, TypeKind::ADT, ident, refs),
    SubstitutionRef (std::move (subst_refs), std::move (generic_arguments),
		     region_constraints),
    id (id), identifier (identifier), variants (variants), adt_kind (adt_kind),
    repr (repr)
{}

void
ADTType::accept_vis (TyVisitor &vis)
{
  vis.visit (*this);
}

void
ADTType::accept_vis (TyConstVisitor &vis) const
{
  vis.visit (*this);
}

std::string
ADTType::as_string () const
{
  std::string variants_buffer;
  for (size_t i = 0; i < number_of_variants (); ++i)
    {
      TyTy::VariantDef *variant = variants.at (i);
      variants_buffer += variant->as_string ();
      if ((i + 1) < number_of_variants ())
	variants_buffer += ", ";
    }

  return identifier + subst_as_string () + "{" + variants_buffer + "}";
}

bool
ADTType::can_eq (const BaseType *other, bool emit_errors) const
{
  ADTCmp r (this, emit_errors);
  return r.can_eq (other);
}

bool
ADTType::is_equal (const BaseType &other) const
{
  if (get_kind () != other.get_kind ())
    return false;

  auto other2 = other.as<const ADTType> ();
  if (get_adt_kind () != other2->get_adt_kind ())
    return false;

  if (number_of_variants () != other2->number_of_variants ())
    return false;

  if (has_substitutions_defined () != other2->has_substitutions_defined ())
    return false;

  if (has_substitutions_defined ())
    {
      if (get_num_substitutions () != other2->get_num_substitutions ())
	return false;

      for (size_t i = 0; i < get_num_substitutions (); i++)
	{
	  const SubstitutionParamMapping &a = substitutions.at (i);
	  const SubstitutionParamMapping &b = other2->substitutions.at (i);

	  const ParamType *aa = a.get_param_ty ();
	  const ParamType *bb = b.get_param_ty ();
	  BaseType *aaa = aa->resolve ();
	  BaseType *bbb = bb->resolve ();
	  if (!aaa->is_equal (*bbb))
	    return false;
	}
    }

  for (size_t i = 0; i < number_of_variants (); i++)
    {
      const TyTy::VariantDef *a = get_variants ().at (i);
      const TyTy::VariantDef *b = other2->get_variants ().at (i);

      if (!a->is_equal (*b))
	return false;
    }

  return true;
}

DefId
ADTType::get_id () const
{
  return id;
}

BaseType *
ADTType::clone () const
{
  std::vector<VariantDef *> cloned_variants;
  for (auto &variant : variants)
    cloned_variants.push_back (variant->clone ());

  return new ADTType (get_id (), get_ref (), get_ty_ref (), identifier, ident,
		      get_adt_kind (), cloned_variants, clone_substs (),
		      get_repr_options (), used_arguments,
		      get_region_constraints (), get_combined_refs ());
}

static bool
handle_substitions (SubstitutionArgumentMappings &subst_mappings,
		    StructFieldType *field)
{
  auto fty = field->get_field_type ();
  if (auto p = fty->try_as<ParamType> ())
    {
      SubstitutionArg arg = SubstitutionArg::error ();
      bool ok = subst_mappings.get_argument_for_symbol (p, &arg);
      if (ok)
	{
	  auto argt = arg.get_tyty ();
	  bool arg_is_param = argt->get_kind () == TyTy::TypeKind::PARAM;
	  bool arg_is_concrete = argt->get_kind () != TyTy::TypeKind::INFER;

	  if (arg_is_param || arg_is_concrete)
	    {
	      auto new_field = argt->clone ();
	      new_field->set_ref (fty->get_ref ());
	      field->set_field_type (new_field);
	    }
	  else
	    {
	      field->get_field_type ()->set_ty_ref (argt->get_ref ());
	    }
	}
    }
  else if (fty->has_substitutions_defined () || !fty->is_concrete ())
    {
      BaseType *concrete
	= Resolver::SubstMapperInternal::Resolve (fty, subst_mappings);

      if (concrete->get_kind () == TyTy::TypeKind::ERROR)
	{
	  rust_error_at (subst_mappings.get_locus (),
			 "Failed to resolve field substitution type: %s",
			 fty->as_string ().c_str ());
	  return false;
	}

      auto new_field = concrete->clone ();
      new_field->set_ref (fty->get_ref ());
      field->set_field_type (new_field);
    }

  return true;
}

ADTType *
ADTType::handle_substitions (SubstitutionArgumentMappings &subst_mappings)
{
  auto adt = clone ()->as<ADTType> ();
  adt->set_ty_ref (mappings.get_next_hir_id ());
  adt->used_arguments = subst_mappings;

  for (auto &sub : adt->get_substs ())
    {
      SubstitutionArg arg = SubstitutionArg::error ();
      bool ok
	= subst_mappings.get_argument_for_symbol (sub.get_param_ty (), &arg);
      if (ok)
	sub.fill_param_ty (subst_mappings, subst_mappings.get_locus ());
    }

  for (auto &variant : adt->get_variants ())
    {
      if (variant->is_dataless_variant ())
	continue;

      for (auto &field : variant->get_fields ())
	{
	  bool ok = ::Rust::TyTy::handle_substitions (subst_mappings, field);
	  if (!ok)
	    return adt;
	}
    }

  return adt;
}

// TupleType

TupleType::TupleType (HirId ref, location_t locus, std::vector<TyVar> fields,
		      std::set<HirId> refs)
  : BaseType (ref, ref, KIND, {Resolver::CanonicalPath::create_empty (), locus},
	      refs),
    fields (fields)
{}

TupleType::TupleType (HirId ref, HirId ty_ref, location_t locus,
		      std::vector<TyVar> fields, std::set<HirId> refs)
  : BaseType (ref, ty_ref, KIND,
	      {Resolver::CanonicalPath::create_empty (), locus}, refs),
    fields (fields)
{}

TupleType *
TupleType::get_unit_type ()
{
  static TupleType *ret = nullptr;
  if (ret == nullptr)
    ret = new TupleType (Analysis::Mappings::get ().get_next_hir_id (),
			 BUILTINS_LOCATION);
  return ret;
}

size_t
TupleType::num_fields () const
{
  return fields.size ();
}

const std::vector<TyVar> &
TupleType::get_fields () const
{
  return fields;
}

void
TupleType::accept_vis (TyVisitor &vis)
{
  vis.visit (*this);
}

void
TupleType::accept_vis (TyConstVisitor &vis) const
{
  vis.visit (*this);
}

std::string
TupleType::as_string () const
{
  size_t i = 0;
  std::string fields_buffer;
  for (const TyVar &field : get_fields ())
    {
      fields_buffer += field.get_tyty ()->as_string ();
      bool has_next = (i + 1) < get_fields ().size ();
      fields_buffer += has_next ? ", " : "";
      i++;
    }
  return "(" + fields_buffer + ")";
}

std::string
TupleType::get_name () const
{
  size_t i = 0;
  std::string fields_buffer;
  for (const TyVar &field : get_fields ())
    {
      fields_buffer += field.get_tyty ()->as_string ();
      bool has_next = (i + 1) < get_fields ().size ();
      fields_buffer += has_next ? ", " : "";
      i++;
    }
  return "(" + fields_buffer + ")";
}

BaseType *
TupleType::get_field (size_t index) const
{
  return fields.at (index).get_tyty ();
}

bool
TupleType::can_eq (const BaseType *other, bool emit_errors) const
{
  TupleCmp r (this, emit_errors);
  return r.can_eq (other);
}

bool
TupleType::is_equal (const BaseType &other) const
{
  if (get_kind () != other.get_kind ())
    return false;

  auto other2 = other.as<const TupleType> ();
  if (num_fields () != other2->num_fields ())
    return false;

  for (size_t i = 0; i < num_fields (); i++)
    {
      if (!get_field (i)->is_equal (*other2->get_field (i)))
	return false;
    }
  return true;
}

BaseType *
TupleType::clone () const
{
  std::vector<TyVar> cloned_fields;
  for (const auto &f : fields)
    cloned_fields.push_back (f.clone ());

  return new TupleType (get_ref (), get_ty_ref (), get_ident ().locus,
			cloned_fields, get_combined_refs ());
}

TupleType *
TupleType::handle_substitions (SubstitutionArgumentMappings &mappings)
{
  auto &mappings_table = Analysis::Mappings::get ();

  auto tuple = clone ()->as<TupleType> ();
  tuple->set_ref (mappings_table.get_next_hir_id ());
  tuple->set_ty_ref (mappings_table.get_next_hir_id ());

  for (size_t i = 0; i < tuple->fields.size (); i++)
    {
      TyVar &field = fields.at (i);
      if (!field.get_tyty ()->is_concrete ())
	{
	  BaseType *concrete
	    = Resolver::SubstMapperInternal::Resolve (field.get_tyty (),
						      mappings);
	  tuple->fields[i]
	    = TyVar::subst_covariant_var (field.get_tyty (), concrete);
	}
    }

  return tuple;
}

void
FnType::accept_vis (TyVisitor &vis)
{
  vis.visit (*this);
}

void
FnType::accept_vis (TyConstVisitor &vis) const
{
  vis.visit (*this);
}

std::string
FnType::as_string () const
{
  std::string params_str = "";
  for (auto &param : params)
    {
      auto &pattern = param.get_pattern ();
      auto ty = param.get_type ();
      params_str += pattern.as_string () + " " + ty->as_string ();
      params_str += ",";
    }

  std::string ret_str = type->as_string ();
  return "fn" + subst_as_string () + " (" + params_str + ") -> " + ret_str;
}

bool
FnType::can_eq (const BaseType *other, bool emit_errors) const
{
  FnCmp r (this, emit_errors);
  return r.can_eq (other);
}

bool
FnType::is_equal (const BaseType &other) const
{
  if (get_kind () != other.get_kind ())
    return false;

  auto &other2 = static_cast<const FnType &> (other);
  if (get_identifier ().compare (other2.get_identifier ()) != 0)
    return false;

  if (!get_return_type ()->is_equal (*other2.get_return_type ()))
    return false;

  if (has_substitutions_defined () != other2.has_substitutions_defined ())
    return false;

  if (has_substitutions_defined ())
    {
      if (get_num_substitutions () != other2.get_num_substitutions ())
	return false;

      const FnType &ofn = static_cast<const FnType &> (other);
      for (size_t i = 0; i < get_num_substitutions (); i++)
	{
	  const SubstitutionParamMapping &a = get_substs ().at (i);
	  const SubstitutionParamMapping &b = ofn.get_substs ().at (i);

	  const ParamType *pa = a.get_param_ty ();
	  const ParamType *pb = b.get_param_ty ();

	  if (!pa->is_equal (*pb))
	    return false;
	}
    }

  if (num_params () != other2.num_params ())
    return false;

  for (size_t i = 0; i < num_params (); i++)
    {
      auto lhs = param_at (i).get_type ();
      auto rhs = other2.param_at (i).get_type ();
      if (!lhs->is_equal (*rhs))
	return false;
    }
  return true;
}

BaseType *
FnType::clone () const
{
  std::vector<TyTy::FnParam> cloned_params;
  for (auto &p : params)
    cloned_params.push_back (p.clone ());

  return new FnType (get_ref (), get_ty_ref (), get_id (), get_identifier (),
		     ident, flags, abi, std::move (cloned_params),
		     get_return_type ()->clone (), clone_substs (),
		     get_substitution_arguments (), get_region_constraints (),
		     get_combined_refs ());
}

FnType *
FnType::handle_substitions (SubstitutionArgumentMappings &subst_mappings)
{
  FnType *fn = static_cast<FnType *> (clone ());
  fn->set_ty_ref (mappings.get_next_hir_id ());
  fn->used_arguments = subst_mappings;

  for (auto &sub : fn->get_substs ())
    {
      SubstitutionArg arg = SubstitutionArg::error ();

      bool ok
	= subst_mappings.get_argument_for_symbol (sub.get_param_ty (), &arg);
      if (ok)
	{
	  sub.fill_param_ty (subst_mappings, subst_mappings.get_locus ());
	}
    }

  auto fty = fn->get_return_type ();
  bool is_param_ty = fty->get_kind () == TypeKind::PARAM;
  if (is_param_ty)
    {
      ParamType *p = static_cast<ParamType *> (fty);

      SubstitutionArg arg = SubstitutionArg::error ();
      bool ok = subst_mappings.get_argument_for_symbol (p, &arg);
      if (ok)
	{
	  auto argt = arg.get_tyty ();
	  bool arg_is_param = argt->get_kind () == TyTy::TypeKind::PARAM;
	  bool arg_is_concrete = argt->get_kind () != TyTy::TypeKind::INFER;

	  if (arg_is_param || arg_is_concrete)
	    {
	      auto new_field = argt->clone ();
	      new_field->set_ref (fty->get_ref ());
	      fn->type = new_field;
	    }
	  else
	    {
	      fty->set_ty_ref (argt->get_ref ());
	    }
	}
    }
  else if (fty->needs_generic_substitutions () || !fty->is_concrete ())
    {
      BaseType *concrete
	= Resolver::SubstMapperInternal::Resolve (fty, subst_mappings);

      if (concrete == nullptr || concrete->get_kind () == TyTy::TypeKind::ERROR)
	{
	  rust_error_at (subst_mappings.get_locus (),
			 "Failed to resolve field substitution type: %s",
			 fty->as_string ().c_str ());
	  return nullptr;
	}

      auto new_field = concrete->clone ();
      new_field->set_ref (fty->get_ref ());
      fn->type = new_field;
    }

  for (auto &param : fn->get_params ())
    {
      auto fty = param.get_type ();

      bool is_param_ty = fty->get_kind () == TypeKind::PARAM;
      if (is_param_ty)
	{
	  ParamType *p = static_cast<ParamType *> (fty);

	  SubstitutionArg arg = SubstitutionArg::error ();
	  bool ok = subst_mappings.get_argument_for_symbol (p, &arg);
	  if (ok)
	    {
	      auto argt = arg.get_tyty ();
	      bool arg_is_param = argt->get_kind () == TyTy::TypeKind::PARAM;
	      bool arg_is_concrete = argt->get_kind () != TyTy::TypeKind::INFER;

	      if (arg_is_param || arg_is_concrete)
		{
		  auto new_field = argt->clone ();
		  new_field->set_ref (fty->get_ref ());
		  param.set_type (new_field);
		}
	      else
		{
		  fty->set_ty_ref (argt->get_ref ());
		}
	    }
	}
      else if (fty->has_substitutions_defined () || !fty->is_concrete ())
	{
	  BaseType *concrete
	    = Resolver::SubstMapperInternal::Resolve (fty, subst_mappings);

	  if (concrete == nullptr
	      || concrete->get_kind () == TyTy::TypeKind::ERROR)
	    {
	      rust_error_at (subst_mappings.get_locus (),
			     "Failed to resolve field substitution type: %s",
			     fty->as_string ().c_str ());
	      return nullptr;
	    }

	  auto new_field = concrete->clone ();
	  new_field->set_ref (fty->get_ref ());
	  param.set_type (new_field);
	}
    }

  return fn;
}

void
FnPtr::accept_vis (TyVisitor &vis)
{
  vis.visit (*this);
}

void
FnPtr::accept_vis (TyConstVisitor &vis) const
{
  vis.visit (*this);
}

std::string
FnPtr::as_string () const
{
  std::string params_str;

  auto &params = get_params ();
  for (auto &p : params)
    {
      params_str += p.get_tyty ()->as_string () + " ,";
    }

  return "fnptr (" + params_str + ") -> " + get_return_type ()->as_string ();
}

bool
FnPtr::can_eq (const BaseType *other, bool emit_errors) const
{
  FnptrCmp r (this, emit_errors);
  return r.can_eq (other);
}

bool
FnPtr::is_equal (const BaseType &other) const
{
  if (get_kind () != other.get_kind ())
    return false;

  auto other2 = static_cast<const FnPtr &> (other);
  auto this_ret_type = get_return_type ();
  auto other_ret_type = other2.get_return_type ();
  if (this_ret_type->is_equal (*other_ret_type))
    return false;

  if (num_params () != other2.num_params ())
    return false;

  for (size_t i = 0; i < num_params (); i++)
    {
      if (!get_param_type_at (i)->is_equal (*other2.get_param_type_at (i)))
	return false;
    }
  return true;
}

BaseType *
FnPtr::clone () const
{
  std::vector<TyVar> cloned_params;
  for (auto &p : params)
    cloned_params.push_back (TyVar (p.get_ref ()));

  return new FnPtr (get_ref (), get_ty_ref (), ident.locus,
		    std::move (cloned_params), result_type,
		    get_combined_refs ());
}

void
ClosureType::accept_vis (TyVisitor &vis)
{
  vis.visit (*this);
}

void
ClosureType::accept_vis (TyConstVisitor &vis) const
{
  vis.visit (*this);
}

std::string
ClosureType::as_string () const
{
  std::string params_buf = parameters->as_string ();
  return "|" + params_buf + "| {" + result_type.get_tyty ()->as_string () + "}";
}

bool
ClosureType::can_eq (const BaseType *other, bool emit_errors) const
{
  ClosureCmp r (this, emit_errors);
  return r.can_eq (other);
}

bool
ClosureType::is_equal (const BaseType &other) const
{
  if (other.get_kind () != TypeKind::CLOSURE)
    return false;

  const ClosureType &other2 = static_cast<const ClosureType &> (other);
  if (get_def_id () != other2.get_def_id ())
    return false;

  if (!get_parameters ().is_equal (other2.get_parameters ()))
    return false;

  return get_result_type ().is_equal (other2.get_result_type ());
}

BaseType *
ClosureType::clone () const
{
  return new ClosureType (get_ref (), get_ty_ref (), ident, id,
			  (TyTy::TupleType *) parameters->clone (), result_type,
			  clone_substs (), captures, get_combined_refs (),
			  specified_bounds);
}

ClosureType *
ClosureType::handle_substitions (SubstitutionArgumentMappings &mappings)
{
  rust_unreachable ();
  return nullptr;
}

void
ClosureType::setup_fn_once_output () const
{
  // lookup the lang items
  auto fn_once_lookup = mappings.lookup_lang_item (LangItem::Kind::FN_ONCE);
  auto fn_once_output_lookup
    = mappings.lookup_lang_item (LangItem::Kind::FN_ONCE_OUTPUT);
  if (!fn_once_lookup)
    {
      rust_fatal_error (UNKNOWN_LOCATION,
			"Missing required %<fn_once%> lang item");
      return;
    }
  if (!fn_once_output_lookup)
    {
      rust_fatal_error (UNKNOWN_LOCATION,
			"Missing required %<fn_once_ouput%> lang item");
      return;
    }

  DefId &trait_id = fn_once_lookup.value ();
  DefId &trait_item_id = fn_once_output_lookup.value ();

  // resolve to the trait
  HIR::Item *item = mappings.lookup_defid (trait_id).value ();
  rust_assert (item->get_item_kind () == HIR::Item::ItemKind::Trait);
  HIR::Trait *trait = static_cast<HIR::Trait *> (item);

  Resolver::TraitReference *trait_ref
    = Resolver::TraitResolver::Resolve (*trait);
  rust_assert (!trait_ref->is_error ());

  // resolve to trait item
  HIR::TraitItem *trait_item
    = mappings.lookup_trait_item_defid (trait_item_id).value ();
  rust_assert (trait_item->get_item_kind ()
	       == HIR::TraitItem::TraitItemKind::TYPE);
  std::string item_identifier = trait_item->trait_identifier ();

  // setup associated types  #[lang = "fn_once_output"]
  Resolver::TraitItemReference *item_reference = nullptr;
  bool found = trait_ref->lookup_trait_item_by_type (
    item_identifier, Resolver::TraitItemReference::TraitItemType::TYPE,
    &item_reference);
  rust_assert (found);

  // setup
  item_reference->associated_type_set (&get_result_type ());
}

void
ArrayType::accept_vis (TyVisitor &vis)
{
  vis.visit (*this);
}

void
ArrayType::accept_vis (TyConstVisitor &vis) const
{
  vis.visit (*this);
}

std::string
ArrayType::as_string () const
{
  return "[" + get_element_type ()->as_string () + ":" + "CAPACITY" + "]";
}

bool
ArrayType::can_eq (const BaseType *other, bool emit_errors) const
{
  ArrayCmp r (this, emit_errors);
  return r.can_eq (other);
}

bool
ArrayType::is_equal (const BaseType &other) const
{
  if (get_kind () != other.get_kind ())
    return false;

  auto other2 = static_cast<const ArrayType &> (other);

  auto this_element_type = get_element_type ();
  auto other_element_type = other2.get_element_type ();

  return this_element_type->is_equal (*other_element_type);
}

BaseType *
ArrayType::get_element_type () const
{
  return element_type.get_tyty ();
}

const TyVar &
ArrayType::get_var_element_type () const
{
  return element_type;
}

BaseType *
ArrayType::clone () const
{
  return new ArrayType (get_ref (), get_ty_ref (), ident.locus, capacity_expr,
			element_type, get_combined_refs ());
}

ArrayType *
ArrayType::handle_substitions (SubstitutionArgumentMappings &mappings)
{
  auto &mappings_table = Analysis::Mappings::get ();

  ArrayType *ref = static_cast<ArrayType *> (clone ());
  ref->set_ty_ref (mappings_table.get_next_hir_id ());

  // might be &T or &ADT so this needs to be recursive
  auto base = ref->get_element_type ();
  BaseType *concrete = Resolver::SubstMapperInternal::Resolve (base, mappings);
  ref->element_type = TyVar::subst_covariant_var (base, concrete);

  return ref;
}

void
SliceType::accept_vis (TyVisitor &vis)
{
  vis.visit (*this);
}

void
SliceType::accept_vis (TyConstVisitor &vis) const
{
  vis.visit (*this);
}

std::string
SliceType::as_string () const
{
  return "[" + get_element_type ()->as_string () + "]";
}

bool
SliceType::can_eq (const BaseType *other, bool emit_errors) const
{
  SliceCmp r (this, emit_errors);
  return r.can_eq (other);
}

bool
SliceType::is_equal (const BaseType &other) const
{
  if (get_kind () != other.get_kind ())
    return false;

  auto other2 = static_cast<const SliceType &> (other);

  auto this_element_type = get_element_type ();
  auto other_element_type = other2.get_element_type ();

  return this_element_type->is_equal (*other_element_type);
}

BaseType *
SliceType::get_element_type () const
{
  return element_type.get_tyty ();
}

const TyVar &
SliceType::get_var_element_type () const
{
  return element_type;
}

BaseType *
SliceType::clone () const
{
  return new SliceType (get_ref (), get_ty_ref (), ident.locus,
			element_type.clone (), get_combined_refs ());
}

SliceType *
SliceType::handle_substitions (SubstitutionArgumentMappings &mappings)
{
  auto &mappings_table = Analysis::Mappings::get ();

  SliceType *ref = static_cast<SliceType *> (clone ());
  ref->set_ty_ref (mappings_table.get_next_hir_id ());

  // might be &T or &ADT so this needs to be recursive
  auto base = ref->get_element_type ();
  BaseType *concrete = Resolver::SubstMapperInternal::Resolve (base, mappings);
  ref->element_type = TyVar::subst_covariant_var (base, concrete);

  return ref;
}

// BoolType

BoolType::BoolType (HirId ref, std::set<HirId> refs)
  : BaseType (ref, ref, KIND,
	      {Resolver::CanonicalPath::create_empty (), BUILTINS_LOCATION},
	      refs)
{}

BoolType::BoolType (HirId ref, HirId ty_ref, std::set<HirId> refs)
  : BaseType (ref, ty_ref, KIND,
	      {Resolver::CanonicalPath::create_empty (), BUILTINS_LOCATION},
	      refs)
{}

std::string
BoolType::get_name () const
{
  return as_string ();
}

void
BoolType::accept_vis (TyVisitor &vis)
{
  vis.visit (*this);
}

void
BoolType::accept_vis (TyConstVisitor &vis) const
{
  vis.visit (*this);
}

std::string
BoolType::as_string () const
{
  return "bool";
}

bool
BoolType::can_eq (const BaseType *other, bool emit_errors) const
{
  BoolCmp r (this, emit_errors);
  return r.can_eq (other);
}

BaseType *
BoolType::clone () const
{
  return new BoolType (get_ref (), get_ty_ref (), get_combined_refs ());
}

// IntType

IntType::IntType (HirId ref, IntKind kind, std::set<HirId> refs)
  : BaseType (ref, ref, KIND,
	      {Resolver::CanonicalPath::create_empty (), BUILTINS_LOCATION},
	      refs),
    int_kind (kind)
{}

IntType::IntType (HirId ref, HirId ty_ref, IntKind kind, std::set<HirId> refs)
  : BaseType (ref, ty_ref, KIND,
	      {Resolver::CanonicalPath::create_empty (), BUILTINS_LOCATION},
	      refs),
    int_kind (kind)
{}

std::string
IntType::get_name () const
{
  return as_string ();
}

IntType::IntKind
IntType::get_int_kind () const
{
  return int_kind;
}

void
IntType::accept_vis (TyVisitor &vis)
{
  vis.visit (*this);
}

void
IntType::accept_vis (TyConstVisitor &vis) const
{
  vis.visit (*this);
}

std::string
IntType::as_string () const
{
  switch (int_kind)
    {
    case I8:
      return "i8";
    case I16:
      return "i16";
    case I32:
      return "i32";
    case I64:
      return "i64";
    case I128:
      return "i128";
    }
  rust_unreachable ();
  return "__unknown_int_type";
}

bool
IntType::can_eq (const BaseType *other, bool emit_errors) const
{
  IntCmp r (this, emit_errors);
  return r.can_eq (other);
}

BaseType *
IntType::clone () const
{
  return new IntType (get_ref (), get_ty_ref (), get_int_kind (),
		      get_combined_refs ());
}

bool
IntType::is_equal (const BaseType &other) const
{
  if (!BaseType::is_equal (other))
    return false;

  const IntType &o = static_cast<const IntType &> (other);
  return get_int_kind () == o.get_int_kind ();
}

// UintType

UintType::UintType (HirId ref, UintKind kind, std::set<HirId> refs)
  : BaseType (ref, ref, KIND,
	      {Resolver::CanonicalPath::create_empty (), BUILTINS_LOCATION},
	      refs),
    uint_kind (kind)
{}

UintType::UintType (HirId ref, HirId ty_ref, UintKind kind,
		    std::set<HirId> refs)
  : BaseType (ref, ty_ref, KIND,
	      {Resolver::CanonicalPath::create_empty (), BUILTINS_LOCATION},
	      refs),
    uint_kind (kind)
{}

std::string
UintType::get_name () const
{
  return as_string ();
}

UintType::UintKind
UintType::get_uint_kind () const
{
  return uint_kind;
}

void
UintType::accept_vis (TyVisitor &vis)
{
  vis.visit (*this);
}

void
UintType::accept_vis (TyConstVisitor &vis) const
{
  vis.visit (*this);
}

std::string
UintType::as_string () const
{
  switch (uint_kind)
    {
    case U8:
      return "u8";
    case U16:
      return "u16";
    case U32:
      return "u32";
    case U64:
      return "u64";
    case U128:
      return "u128";
    }
  rust_unreachable ();
  return "__unknown_uint_type";
}

bool
UintType::can_eq (const BaseType *other, bool emit_errors) const
{
  UintCmp r (this, emit_errors);
  return r.can_eq (other);
}

BaseType *
UintType::clone () const
{
  return new UintType (get_ref (), get_ty_ref (), get_uint_kind (),
		       get_combined_refs ());
}

bool
UintType::is_equal (const BaseType &other) const
{
  if (!BaseType::is_equal (other))
    return false;

  const UintType &o = static_cast<const UintType &> (other);
  return get_uint_kind () == o.get_uint_kind ();
}

// FloatType

FloatType::FloatType (HirId ref, FloatKind kind, std::set<HirId> refs)
  : BaseType (ref, ref, KIND,
	      {Resolver::CanonicalPath::create_empty (), BUILTINS_LOCATION},
	      refs),
    float_kind (kind)
{}

FloatType::FloatType (HirId ref, HirId ty_ref, FloatKind kind,
		      std::set<HirId> refs)
  : BaseType (ref, ty_ref, KIND,
	      {Resolver::CanonicalPath::create_empty (), BUILTINS_LOCATION},
	      refs),
    float_kind (kind)
{}

std::string
FloatType::get_name () const
{
  return as_string ();
}

FloatType::FloatKind
FloatType::get_float_kind () const
{
  return float_kind;
}

void
FloatType::accept_vis (TyVisitor &vis)
{
  vis.visit (*this);
}

void
FloatType::accept_vis (TyConstVisitor &vis) const
{
  vis.visit (*this);
}

std::string
FloatType::as_string () const
{
  switch (float_kind)
    {
    case F32:
      return "f32";
    case F64:
      return "f64";
    }
  rust_unreachable ();
  return "__unknown_float_type";
}

bool
FloatType::can_eq (const BaseType *other, bool emit_errors) const
{
  FloatCmp r (this, emit_errors);
  return r.can_eq (other);
}

BaseType *
FloatType::clone () const
{
  return new FloatType (get_ref (), get_ty_ref (), get_float_kind (),
			get_combined_refs ());
}

bool
FloatType::is_equal (const BaseType &other) const
{
  if (!BaseType::is_equal (other))
    return false;

  const FloatType &o = static_cast<const FloatType &> (other);
  return get_float_kind () == o.get_float_kind ();
}

// UsizeType

USizeType::USizeType (HirId ref, std::set<HirId> refs)
  : BaseType (ref, ref, KIND,
	      {Resolver::CanonicalPath::create_empty (), BUILTINS_LOCATION},
	      refs)
{}

USizeType::USizeType (HirId ref, HirId ty_ref, std::set<HirId> refs)
  : BaseType (ref, ty_ref, KIND,
	      {Resolver::CanonicalPath::create_empty (), BUILTINS_LOCATION},
	      refs)
{}

std::string
USizeType::get_name () const
{
  return as_string ();
}

void
USizeType::accept_vis (TyVisitor &vis)
{
  vis.visit (*this);
}

void
USizeType::accept_vis (TyConstVisitor &vis) const
{
  vis.visit (*this);
}

std::string
USizeType::as_string () const
{
  return "usize";
}

bool
USizeType::can_eq (const BaseType *other, bool emit_errors) const
{
  USizeCmp r (this, emit_errors);
  return r.can_eq (other);
}

BaseType *
USizeType::clone () const
{
  return new USizeType (get_ref (), get_ty_ref (), get_combined_refs ());
}

// ISizeType

ISizeType::ISizeType (HirId ref, std::set<HirId> refs)
  : BaseType (ref, ref, KIND,
	      {Resolver::CanonicalPath::create_empty (), BUILTINS_LOCATION},
	      refs)
{}

ISizeType::ISizeType (HirId ref, HirId ty_ref, std::set<HirId> refs)
  : BaseType (ref, ty_ref, KIND,
	      {Resolver::CanonicalPath::create_empty (), BUILTINS_LOCATION},
	      refs)
{}

std::string
ISizeType::get_name () const
{
  return as_string ();
}

void
ISizeType::accept_vis (TyVisitor &vis)
{
  vis.visit (*this);
}

void
ISizeType::accept_vis (TyConstVisitor &vis) const
{
  vis.visit (*this);
}

std::string
ISizeType::as_string () const
{
  return "isize";
}

bool
ISizeType::can_eq (const BaseType *other, bool emit_errors) const
{
  ISizeCmp r (this, emit_errors);
  return r.can_eq (other);
}

BaseType *
ISizeType::clone () const
{
  return new ISizeType (get_ref (), get_ty_ref (), get_combined_refs ());
}

// Char Type

CharType::CharType (HirId ref, std::set<HirId> refs)
  : BaseType (ref, ref, KIND,
	      {Resolver::CanonicalPath::create_empty (), BUILTINS_LOCATION},
	      refs)
{}

CharType::CharType (HirId ref, HirId ty_ref, std::set<HirId> refs)
  : BaseType (ref, ty_ref, KIND,
	      {Resolver::CanonicalPath::create_empty (), BUILTINS_LOCATION},
	      refs)
{}

std::string
CharType::get_name () const
{
  return as_string ();
}

void
CharType::accept_vis (TyVisitor &vis)
{
  vis.visit (*this);
}

void
CharType::accept_vis (TyConstVisitor &vis) const
{
  vis.visit (*this);
}

std::string
CharType::as_string () const
{
  return "char";
}

bool
CharType::can_eq (const BaseType *other, bool emit_errors) const
{
  CharCmp r (this, emit_errors);
  return r.can_eq (other);
}

BaseType *
CharType::clone () const
{
  return new CharType (get_ref (), get_ty_ref (), get_combined_refs ());
}

// Reference Type

ReferenceType::ReferenceType (HirId ref, TyVar base, Mutability mut,
			      Region region, std::set<HirId> refs)
  : BaseType (ref, ref, KIND,
	      {Resolver::CanonicalPath::create_empty (), BUILTINS_LOCATION},
	      std::move (refs)),
    base (base), mut (mut), region (region)
{}

ReferenceType::ReferenceType (HirId ref, HirId ty_ref, TyVar base,
			      Mutability mut, Region region,
			      std::set<HirId> refs)
  : BaseType (ref, ty_ref, KIND,
	      {Resolver::CanonicalPath::create_empty (), BUILTINS_LOCATION},
	      std::move (refs)),
    base (base), mut (mut), region (region)
{}

Mutability
ReferenceType::mutability () const
{
  return mut;
}

bool
ReferenceType::is_mutable () const
{
  return mut == Mutability::Mut;
}
Region
ReferenceType::get_region () const
{
  return region;
}

bool
ReferenceType::is_dyn_object () const
{
  return is_dyn_slice_type () || is_dyn_str_type () || is_dyn_obj_type ();
}

bool
ReferenceType::is_dyn_slice_type (const TyTy::SliceType **slice) const
{
  const TyTy::BaseType *element = get_base ()->destructure ();
  if (element->get_kind () != TyTy::TypeKind::SLICE)
    return false;
  if (slice == nullptr)
    return true;

  *slice = static_cast<const TyTy::SliceType *> (element);
  return true;
}

bool
ReferenceType::is_dyn_str_type (const TyTy::StrType **str) const
{
  const TyTy::BaseType *element = get_base ()->destructure ();
  if (element->get_kind () != TyTy::TypeKind::STR)
    return false;
  if (str == nullptr)
    return true;

  *str = static_cast<const TyTy::StrType *> (element);
  return true;
}

bool
ReferenceType::is_dyn_obj_type (const TyTy::DynamicObjectType **dyn) const
{
  const TyTy::BaseType *element = get_base ()->destructure ();
  if (element->get_kind () != TyTy::TypeKind::DYNAMIC)
    return false;
  if (dyn == nullptr)
    return true;

  *dyn = static_cast<const TyTy::DynamicObjectType *> (element);
  return true;
}

void
ReferenceType::accept_vis (TyVisitor &vis)
{
  vis.visit (*this);
}

void
ReferenceType::accept_vis (TyConstVisitor &vis) const
{
  vis.visit (*this);
}

std::string
ReferenceType::as_string () const
{
  return std::string ("&") + (is_mutable () ? "mut" : "") + " "
	 + get_base ()->as_string ();
}

std::string
ReferenceType::get_name () const
{
  return std::string ("&") + (is_mutable () ? "mut" : "") + " "
	 + get_base ()->get_name ();
}

bool
ReferenceType::can_eq (const BaseType *other, bool emit_errors) const
{
  ReferenceCmp r (this, emit_errors);
  return r.can_eq (other);
}

bool
ReferenceType::is_equal (const BaseType &other) const
{
  if (get_kind () != other.get_kind ())
    return false;

  auto other2 = static_cast<const ReferenceType &> (other);
  if (mutability () != other2.mutability ())
    return false;

  return get_base ()->is_equal (*other2.get_base ());
}

BaseType *
ReferenceType::get_base () const
{
  return base.get_tyty ();
}

const TyVar &
ReferenceType::get_var_element_type () const
{
  return base;
}

BaseType *
ReferenceType::clone () const
{
  return new ReferenceType (get_ref (), get_ty_ref (), base, mutability (),
			    get_region (), get_combined_refs ());
}

ReferenceType *
ReferenceType::handle_substitions (SubstitutionArgumentMappings &mappings)
{
  auto &mappings_table = Analysis::Mappings::get ();

  ReferenceType *ref = static_cast<ReferenceType *> (clone ());
  ref->set_ty_ref (mappings_table.get_next_hir_id ());

  // might be &T or &ADT so this needs to be recursive
  auto base = ref->get_base ();
  BaseType *concrete = Resolver::SubstMapperInternal::Resolve (base, mappings);
  ref->base = TyVar::subst_covariant_var (base, concrete);

  return ref;
}

// PointerType

PointerType::PointerType (HirId ref, TyVar base, Mutability mut,
			  std::set<HirId> refs)
  : BaseType (ref, ref, KIND,
	      {Resolver::CanonicalPath::create_empty (), BUILTINS_LOCATION},
	      refs),
    base (base), mut (mut)
{}

PointerType::PointerType (HirId ref, HirId ty_ref, TyVar base, Mutability mut,
			  std::set<HirId> refs)
  : BaseType (ref, ty_ref, KIND,
	      {Resolver::CanonicalPath::create_empty (), BUILTINS_LOCATION},
	      refs),
    base (base), mut (mut)
{}

Mutability
PointerType::mutability () const
{
  return mut;
}

bool
PointerType::is_mutable () const
{
  return mut == Mutability::Mut;
}

bool
PointerType::is_const () const
{
  return mut == Mutability::Imm;
}

bool
PointerType::is_dyn_object () const
{
  return is_dyn_slice_type () || is_dyn_str_type () || is_dyn_obj_type ();
}

bool
PointerType::is_dyn_slice_type (const TyTy::SliceType **slice) const
{
  const TyTy::BaseType *element = get_base ()->destructure ();
  if (element->get_kind () != TyTy::TypeKind::SLICE)
    return false;
  if (slice == nullptr)
    return true;

  *slice = static_cast<const TyTy::SliceType *> (element);
  return true;
}

bool
PointerType::is_dyn_str_type (const TyTy::StrType **str) const
{
  const TyTy::BaseType *element = get_base ()->destructure ();
  if (element->get_kind () != TyTy::TypeKind::STR)
    return false;
  if (str == nullptr)
    return true;

  *str = static_cast<const TyTy::StrType *> (element);
  return true;
}

bool
PointerType::is_dyn_obj_type (const TyTy::DynamicObjectType **dyn) const
{
  const TyTy::BaseType *element = get_base ()->destructure ();
  if (element->get_kind () != TyTy::TypeKind::DYNAMIC)
    return false;
  if (dyn == nullptr)
    return true;

  *dyn = static_cast<const TyTy::DynamicObjectType *> (element);
  return true;
}

void
PointerType::accept_vis (TyVisitor &vis)
{
  vis.visit (*this);
}

void
PointerType::accept_vis (TyConstVisitor &vis) const
{
  vis.visit (*this);
}

std::string
PointerType::as_string () const
{
  return std::string ("* ") + (is_mutable () ? "mut" : "const") + " "
	 + get_base ()->as_string ();
}

std::string
PointerType::get_name () const
{
  return std::string ("* ") + (is_mutable () ? "mut" : "const") + " "
	 + get_base ()->get_name ();
}

bool
PointerType::can_eq (const BaseType *other, bool emit_errors) const
{
  PointerCmp r (this, emit_errors);
  return r.can_eq (other);
}

bool
PointerType::is_equal (const BaseType &other) const
{
  if (get_kind () != other.get_kind ())
    return false;

  auto other2 = static_cast<const PointerType &> (other);
  if (mutability () != other2.mutability ())
    return false;

  return get_base ()->is_equal (*other2.get_base ());
}

BaseType *
PointerType::get_base () const
{
  return base.get_tyty ();
}

const TyVar &
PointerType::get_var_element_type () const
{
  return base;
}

BaseType *
PointerType::clone () const
{
  return new PointerType (get_ref (), get_ty_ref (), base, mutability (),
			  get_combined_refs ());
}

PointerType *
PointerType::handle_substitions (SubstitutionArgumentMappings &mappings)
{
  auto &mappings_table = Analysis::Mappings::get ();

  PointerType *ref = static_cast<PointerType *> (clone ());
  ref->set_ty_ref (mappings_table.get_next_hir_id ());

  // might be &T or &ADT so this needs to be recursive
  auto base = ref->get_base ();
  BaseType *concrete = Resolver::SubstMapperInternal::Resolve (base, mappings);
  ref->base = TyVar::subst_covariant_var (base, concrete);

  return ref;
}

// PARAM Type

ParamType::ParamType (std::string symbol, location_t locus, HirId ref,
		      HIR::GenericParam &param,
		      std::vector<TypeBoundPredicate> specified_bounds,
		      std::set<HirId> refs)
  : BaseType (ref, ref, KIND,
	      {Resolver::CanonicalPath::new_seg (UNKNOWN_NODEID, symbol),
	       locus},
	      specified_bounds, refs),
    is_trait_self (false), symbol (symbol), param (param)
{}

ParamType::ParamType (bool is_trait_self, std::string symbol, location_t locus,
		      HirId ref, HirId ty_ref, HIR::GenericParam &param,
		      std::vector<TypeBoundPredicate> specified_bounds,
		      std::set<HirId> refs)
  : BaseType (ref, ty_ref, KIND,
	      {Resolver::CanonicalPath::new_seg (UNKNOWN_NODEID, symbol),
	       locus},
	      specified_bounds, refs),
    is_trait_self (is_trait_self), symbol (symbol), param (param)
{}

HIR::GenericParam &
ParamType::get_generic_param ()
{
  return param;
}

bool
ParamType::can_resolve () const
{
  return get_ref () != get_ty_ref ();
}

void
ParamType::accept_vis (TyVisitor &vis)
{
  vis.visit (*this);
}

void
ParamType::accept_vis (TyConstVisitor &vis) const
{
  vis.visit (*this);
}

std::string
ParamType::as_string () const
{
  if (!can_resolve ())
    {
      return get_symbol () + " REF: " + std::to_string (get_ref ());
    }

  BaseType *lookup = resolve ();
  return get_symbol () + "=" + lookup->as_string ();
}

std::string
ParamType::get_name () const
{
  if (!can_resolve ())
    return get_symbol ();

  return destructure ()->get_name ();
}

bool
ParamType::can_eq (const BaseType *other, bool emit_errors) const
{
  ParamCmp r (this, emit_errors);
  return r.can_eq (other);
}

BaseType *
ParamType::clone () const
{
  return new ParamType (is_trait_self, get_symbol (), ident.locus, get_ref (),
			get_ty_ref (), param, get_specified_bounds (),
			get_combined_refs ());
}

std::string
ParamType::get_symbol () const
{
  return symbol;
}

BaseType *
ParamType::resolve () const
{
  TyVar var (get_ty_ref ());
  BaseType *r = var.get_tyty ();

  while (r->get_kind () == TypeKind::PARAM)
    {
      ParamType *rr = static_cast<ParamType *> (r);
      if (!rr->can_resolve ())
	break;

      TyVar v (rr->get_ty_ref ());
      BaseType *n = v.get_tyty ();

      // fix infinite loop
      if (r == n)
	break;

      r = n;
    }

  if (r->get_kind () == TypeKind::PARAM && (r->get_ref () == r->get_ty_ref ()))
    return TyVar (r->get_ty_ref ()).get_tyty ();

  return r;
}

bool
ParamType::is_equal (const BaseType &other) const
{
  if (get_kind () != other.get_kind ())
    {
      if (!can_resolve ())
	return false;

      return resolve ()->is_equal (other);
    }

  auto other2 = static_cast<const ParamType &> (other);
  if (can_resolve () != other2.can_resolve ())
    return false;

  if (can_resolve ())
    return resolve ()->can_eq (other2.resolve (), false);

  return get_symbol ().compare (other2.get_symbol ()) == 0;
}

ParamType *
ParamType::handle_substitions (SubstitutionArgumentMappings &subst_mappings)
{
  SubstitutionArg arg = SubstitutionArg::error ();
  bool ok = subst_mappings.get_argument_for_symbol (this, &arg);
  if (!ok || arg.is_error ())
    return this;

  ParamType *p = static_cast<ParamType *> (clone ());
  subst_mappings.on_param_subst (*p, arg);

  // there are two cases one where we substitute directly to a new PARAM and
  // otherwise
  if (arg.get_tyty ()->get_kind () == TyTy::TypeKind::PARAM)
    {
      p->set_ty_ref (arg.get_tyty ()->get_ref ());
      return p;
    }

  // this is the new subst that this needs to pass
  p->set_ref (mappings.get_next_hir_id ());
  p->set_ty_ref (arg.get_tyty ()->get_ref ());

  return p;
}

void
ParamType::set_implicit_self_trait ()
{
  is_trait_self = true;
}

bool
ParamType::is_implicit_self_trait () const
{
  return is_trait_self;
}

// OpaqueType

OpaqueType::OpaqueType (location_t locus, HirId ref,
			std::vector<TypeBoundPredicate> specified_bounds,
			std::set<HirId> refs)
  : BaseType (ref, ref, KIND,
	      {Resolver::CanonicalPath::new_seg (UNKNOWN_NODEID, "impl"),
	       locus},
	      specified_bounds, refs)
{}

OpaqueType::OpaqueType (location_t locus, HirId ref, HirId ty_ref,
			std::vector<TypeBoundPredicate> specified_bounds,
			std::set<HirId> refs)
  : BaseType (ref, ty_ref, KIND,
	      {Resolver::CanonicalPath::new_seg (UNKNOWN_NODEID, "impl"),
	       locus},
	      specified_bounds, refs)
{}

bool
OpaqueType::can_resolve () const
{
  return get_ref () != get_ty_ref ();
}

void
OpaqueType::accept_vis (TyVisitor &vis)
{
  vis.visit (*this);
}

void
OpaqueType::accept_vis (TyConstVisitor &vis) const
{
  vis.visit (*this);
}

std::string
OpaqueType::as_string () const
{
  return get_name ();
}

std::string
OpaqueType::get_name () const
{
  return "impl " + raw_bounds_as_name ();
}

bool
OpaqueType::can_eq (const BaseType *other, bool emit_errors) const
{
  OpaqueCmp r (this, emit_errors);
  return r.can_eq (other);
}

BaseType *
OpaqueType::clone () const
{
  return new OpaqueType (ident.locus, get_ref (), get_ty_ref (),
			 get_specified_bounds (), get_combined_refs ());
}

BaseType *
OpaqueType::resolve () const
{
  TyVar var (get_ty_ref ());
  BaseType *r = var.get_tyty ();

  while (r->get_kind () == TypeKind::OPAQUE)
    {
      OpaqueType *rr = static_cast<OpaqueType *> (r);
      if (!rr->can_resolve ())
	break;

      TyVar v (rr->get_ty_ref ());
      BaseType *n = v.get_tyty ();

      // fix infinite loop
      if (r == n)
	break;

      r = n;
    }

  if (r->get_kind () == TypeKind::OPAQUE && (r->get_ref () == r->get_ty_ref ()))
    return TyVar (r->get_ty_ref ()).get_tyty ();

  return r;
}

bool
OpaqueType::is_equal (const BaseType &other) const
{
  auto other2 = static_cast<const OpaqueType &> (other);
  if (can_resolve () != other2.can_resolve ())
    return false;

  if (can_resolve ())
    return resolve ()->can_eq (other2.resolve (), false);

  return bounds_compatible (other, UNDEF_LOCATION, false);
}

OpaqueType *
OpaqueType::handle_substitions (SubstitutionArgumentMappings &subst_mappings)
{
  // SubstitutionArg arg = SubstitutionArg::error ();
  // bool ok = subst_mappings.get_argument_for_symbol (this, &arg);
  // if (!ok || arg.is_error ())
  //   return this;

  // OpaqueType *p = static_cast<OpaqueType *> (clone ());
  // subst_mappings.on_param_subst (*p, arg);

  // // there are two cases one where we substitute directly to a new PARAM and
  // // otherwise
  // if (arg.get_tyty ()->get_kind () == TyTy::TypeKind::PARAM)
  //   {
  //     p->set_ty_ref (arg.get_tyty ()->get_ref ());
  //     return p;
  //   }

  // // this is the new subst that this needs to pass
  // p->set_ref (mappings.get_next_hir_id ());
  // p->set_ty_ref (arg.get_tyty ()->get_ref ());

  // return p;

  rust_unreachable ();
  return nullptr;
}

// StrType

StrType::StrType (HirId ref, std::set<HirId> refs)
  : BaseType (ref, ref, KIND,
	      {Resolver::CanonicalPath::create_empty (), BUILTINS_LOCATION},
	      refs)
{}

StrType::StrType (HirId ref, HirId ty_ref, std::set<HirId> refs)
  : BaseType (ref, ty_ref, KIND,
	      {Resolver::CanonicalPath::create_empty (), BUILTINS_LOCATION},
	      refs)
{}

std::string
StrType::get_name () const
{
  return as_string ();
}

BaseType *
StrType::clone () const
{
  return new StrType (get_ref (), get_ty_ref (), get_combined_refs ());
}

void
StrType::accept_vis (TyVisitor &vis)
{
  vis.visit (*this);
}

void
StrType::accept_vis (TyConstVisitor &vis) const
{
  vis.visit (*this);
}

std::string
StrType::as_string () const
{
  return "str";
}

bool
StrType::can_eq (const BaseType *other, bool emit_errors) const
{
  StrCmp r (this, emit_errors);
  return r.can_eq (other);
}

bool
StrType::is_equal (const BaseType &other) const
{
  return get_kind () == other.get_kind ();
}

// Never Type

NeverType::NeverType (HirId ref, std::set<HirId> refs)
  : BaseType (ref, ref, KIND,
	      {Resolver::CanonicalPath::create_empty (), BUILTINS_LOCATION},
	      refs)
{}

NeverType::NeverType (HirId ref, HirId ty_ref, std::set<HirId> refs)
  : BaseType (ref, ty_ref, KIND,
	      {Resolver::CanonicalPath::create_empty (), BUILTINS_LOCATION},
	      refs)
{}

std::string
NeverType::get_name () const
{
  return as_string ();
}

void
NeverType::accept_vis (TyVisitor &vis)
{
  vis.visit (*this);
}

void
NeverType::accept_vis (TyConstVisitor &vis) const
{
  vis.visit (*this);
}

std::string
NeverType::as_string () const
{
  return "!";
}

bool
NeverType::can_eq (const BaseType *other, bool emit_errors) const
{
  NeverCmp r (this, emit_errors);
  return r.can_eq (other);
}

BaseType *
NeverType::clone () const
{
  return new NeverType (get_ref (), get_ty_ref (), get_combined_refs ());
}

// placeholder type

PlaceholderType::PlaceholderType (std::string symbol, DefId id, HirId ref,
				  std::set<HirId> refs)
  : BaseType (ref, ref, KIND,
	      {Resolver::CanonicalPath::create_empty (), BUILTINS_LOCATION},
	      refs),
    symbol (symbol), defId (id)
{}

PlaceholderType::PlaceholderType (std::string symbol, DefId id, HirId ref,
				  HirId ty_ref, std::set<HirId> refs)
  : BaseType (ref, ty_ref, KIND,
	      {Resolver::CanonicalPath::create_empty (), BUILTINS_LOCATION},
	      refs),
    symbol (symbol), defId (id)
{}

std::string
PlaceholderType::get_name () const
{
  return as_string ();
}

std::string
PlaceholderType::get_symbol () const
{
  return symbol;
}

void
PlaceholderType::accept_vis (TyVisitor &vis)
{
  vis.visit (*this);
}

void
PlaceholderType::accept_vis (TyConstVisitor &vis) const
{
  vis.visit (*this);
}

std::string
PlaceholderType::as_string () const
{
  return "<placeholder:" + (can_resolve () ? resolve ()->as_string () : "")
	 + ">";
}

bool
PlaceholderType::can_eq (const BaseType *other, bool emit_errors) const
{
  PlaceholderCmp r (this, emit_errors);
  return r.can_eq (other);
}

BaseType *
PlaceholderType::clone () const
{
  return new PlaceholderType (get_symbol (), get_def_id (), get_ref (),
			      get_ty_ref (), get_combined_refs ());
}

void
PlaceholderType::set_associated_type (HirId ref)
{
  auto context = Resolver::TypeCheckContext::get ();
  context->insert_associated_type_mapping (get_ty_ref (), ref);
}

void
PlaceholderType::clear_associated_type ()
{
  auto context = Resolver::TypeCheckContext::get ();
  context->clear_associated_type_mapping (get_ty_ref ());
}

bool
PlaceholderType::can_resolve () const
{
  auto context = Resolver::TypeCheckContext::get ();

  BaseType *lookup = nullptr;
  HirId mapping;

  if (!context->lookup_associated_type_mapping (get_ty_ref (), &mapping))
    return false;

  if (!context->lookup_type (mapping, &lookup))
    return false;

  return lookup != nullptr;
}

BaseType *
PlaceholderType::resolve () const
{
  auto context = Resolver::TypeCheckContext::get ();

  HirId mapping;
  bool ok = context->lookup_associated_type_mapping (get_ty_ref (), &mapping);
  rust_assert (ok);

  return TyVar (mapping).get_tyty ();
}

bool
PlaceholderType::is_equal (const BaseType &other) const
{
  if (get_kind () != other.get_kind ())
    {
      if (!can_resolve ())
	return false;

      return resolve ()->is_equal (other);
    }

  auto other2 = static_cast<const PlaceholderType &> (other);
  return get_symbol ().compare (other2.get_symbol ()) == 0;
}

DefId
PlaceholderType::get_def_id () const
{
  return defId;
}

// Projection type

ProjectionType::ProjectionType (
  HirId ref, BaseType *base, const Resolver::TraitReference *trait, DefId item,
  std::vector<SubstitutionParamMapping> subst_refs,
  SubstitutionArgumentMappings generic_arguments,
  RegionConstraints region_constraints, std::set<HirId> refs)
  : BaseType (ref, ref, KIND,
	      {Resolver::CanonicalPath::create_empty (), BUILTINS_LOCATION},
	      std::move (refs)),
    SubstitutionRef (std::move (subst_refs), std::move (generic_arguments),
		     std::move (region_constraints)),
    base (base), trait (trait), item (item)
{}

ProjectionType::ProjectionType (
  HirId ref, HirId ty_ref, BaseType *base,
  const Resolver::TraitReference *trait, DefId item,
  std::vector<SubstitutionParamMapping> subst_refs,
  SubstitutionArgumentMappings generic_arguments,
  RegionConstraints region_constraints, std::set<HirId> refs)
  : BaseType (ref, ty_ref, KIND,
	      {Resolver::CanonicalPath::create_empty (), BUILTINS_LOCATION},
	      refs),
    SubstitutionRef (std::move (subst_refs), std::move (generic_arguments),
		     std::move (region_constraints)),
    base (base), trait (trait), item (item)
{}

std::string
ProjectionType::get_name () const
{
  return as_string ();
}

const BaseType *
ProjectionType::get () const
{
  return base;
}

BaseType *
ProjectionType::get ()
{
  return base;
}

void
ProjectionType::accept_vis (TyVisitor &vis)
{
  vis.visit (*this);
}

void
ProjectionType::accept_vis (TyConstVisitor &vis) const
{
  vis.visit (*this);
}

std::string
ProjectionType::as_string () const
{
  return "<Projection=" + subst_as_string () + "::" + base->as_string () + ">";
}

bool
ProjectionType::can_eq (const BaseType *other, bool emit_errors) const
{
  return base->can_eq (other, emit_errors);
}

BaseType *
ProjectionType::clone () const
{
  return new ProjectionType (get_ref (), get_ty_ref (), base->clone (), trait,
			     item, clone_substs (), used_arguments,
			     region_constraints, get_combined_refs ());
}

ProjectionType *
ProjectionType::handle_substitions (
  SubstitutionArgumentMappings &subst_mappings)
{
  // // do we really need to substitute this?
  // if (base->needs_generic_substitutions () ||
  // base->contains_type_parameters
  // ())
  //   {
  //     return this;
  //   }

  ProjectionType *projection = static_cast<ProjectionType *> (clone ());
  projection->set_ty_ref (mappings.get_next_hir_id ());
  projection->used_arguments = subst_mappings;

  auto context = Resolver::TypeCheckContext::get ();
  context->insert_implicit_type (projection->get_ty_ref (), projection);

  for (auto &sub : projection->get_substs ())
    {
      SubstitutionArg arg = SubstitutionArg::error ();
      bool ok
	= subst_mappings.get_argument_for_symbol (sub.get_param_ty (), &arg);
      if (ok)
	sub.fill_param_ty (subst_mappings, subst_mappings.get_locus ());
    }

  auto fty = projection->base;
  bool is_param_ty = fty->get_kind () == TypeKind::PARAM;
  if (is_param_ty)
    {
      ParamType *p = static_cast<ParamType *> (fty);

      SubstitutionArg arg = SubstitutionArg::error ();
      bool ok = subst_mappings.get_argument_for_symbol (p, &arg);
      if (ok)
	{
	  auto argt = arg.get_tyty ();
	  bool arg_is_param = argt->get_kind () == TyTy::TypeKind::PARAM;
	  bool arg_is_concrete = argt->get_kind () != TyTy::TypeKind::INFER;

	  if (arg_is_param || arg_is_concrete)
	    {
	      auto new_field = argt->clone ();
	      new_field->set_ref (fty->get_ref ());
	      projection->base = new_field;
	    }
	  else
	    {
	      fty->set_ty_ref (argt->get_ref ());
	    }
	}
    }
  else if (fty->needs_generic_substitutions () || !fty->is_concrete ())
    {
      BaseType *concrete
	= Resolver::SubstMapperInternal::Resolve (fty, subst_mappings);

      if (concrete == nullptr || concrete->get_kind () == TyTy::TypeKind::ERROR)
	{
	  rust_error_at (subst_mappings.get_locus (),
			 "Failed to resolve field substitution type: %s",
			 fty->as_string ().c_str ());
	  return nullptr;
	}

      projection->base = concrete;
    }

  return projection;
}

// DynObjectType

DynamicObjectType::DynamicObjectType (
  HirId ref, RustIdent ident, std::vector<TypeBoundPredicate> specified_bounds,
  std::set<HirId> refs)
  : BaseType (ref, ref, KIND, ident, specified_bounds, refs)
{}

DynamicObjectType::DynamicObjectType (
  HirId ref, HirId ty_ref, RustIdent ident,
  std::vector<TypeBoundPredicate> specified_bounds, std::set<HirId> refs)
  : BaseType (ref, ty_ref, KIND, ident, specified_bounds, refs)
{}

void
DynamicObjectType::accept_vis (TyVisitor &vis)
{
  vis.visit (*this);
}

void
DynamicObjectType::accept_vis (TyConstVisitor &vis) const
{
  vis.visit (*this);
}

std::string
DynamicObjectType::as_string () const
{
  return "dyn [" + raw_bounds_as_string () + "]";
}

bool
DynamicObjectType::can_eq (const BaseType *other, bool emit_errors) const
{
  DynamicCmp r (this, emit_errors);
  return r.can_eq (other);
}

BaseType *
DynamicObjectType::clone () const
{
  return new DynamicObjectType (get_ref (), get_ty_ref (), ident,
				specified_bounds, get_combined_refs ());
}

std::string
DynamicObjectType::get_name () const
{
  return "dyn [" + raw_bounds_as_name () + "]";
}

bool
DynamicObjectType::is_equal (const BaseType &other) const
{
  if (get_kind () != other.get_kind ())
    return false;

  if (num_specified_bounds () != other.num_specified_bounds ())
    return false;

  return bounds_compatible (other, UNDEF_LOCATION, false);
}

const std::vector<
  std::pair<const Resolver::TraitItemReference *, const TypeBoundPredicate *>>
DynamicObjectType::get_object_items () const
{
  std::vector<
    std::pair<const Resolver::TraitItemReference *, const TypeBoundPredicate *>>
    items;
  for (const TypeBoundPredicate &bound : get_specified_bounds ())
    {
      const Resolver::TraitReference *trait = bound.get ();
      std::vector<const Resolver::TraitItemReference *> trait_items;
      trait->get_trait_items_and_supers (trait_items);

      for (auto &item : trait_items)
	{
	  if (item->get_trait_item_type ()
		== Resolver::TraitItemReference::TraitItemType::FN
	      && item->is_object_safe ())
	    items.push_back ({item, &bound});
	}
    }
  return items;
}

} // namespace TyTy
} // namespace Rust
