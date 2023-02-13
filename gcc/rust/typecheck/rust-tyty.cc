// Copyright (C) 2020-2023 Free Software Foundation, Inc.

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

#include "rust-hir-type-check-expr.h"
#include "rust-hir-type-check-type.h"
#include "rust-tyty-visitor.h"
#include "rust-tyty-call.h"
#include "rust-hir-map.h"
#include "rust-location.h"
#include "rust-linemap.h"

#include "rust-substitution-mapper.h"
#include "rust-hir-trait-reference.h"
#include "rust-hir-type-bounds.h"
#include "rust-hir-trait-resolve.h"
#include "rust-tyty-cmp.h"
#include "rust-type-util.h"

#include "options.h"

namespace Rust {
namespace TyTy {

bool autoderef_cmp_flag = false;

void
set_cmp_autoderef_mode ()
{
  autoderef_cmp_flag = true;
}
void
reset_cmp_autoderef_mode ()
{
  autoderef_cmp_flag = false;
}

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

    case TypeKind::ERROR:
      return "ERROR";
    }
  gcc_unreachable ();
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
    combined (refs), ident (ident), mappings (Analysis::Mappings::get ())
{}

BaseType::BaseType (HirId ref, HirId ty_ref, TypeKind kind, RustIdent ident,
		    std::vector<TypeBoundPredicate> specified_bounds,
		    std::set<HirId> refs)
  : TypeBoundsMappings (specified_bounds), kind (kind), ref (ref),
    ty_ref (ty_ref), combined (refs), ident (ident),
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

bool
BaseType::is_equal (const BaseType &other) const
{
  return get_kind () == other.get_kind ();
}

bool
BaseType::is_unit () const
{
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

bool
BaseType::supports_substitutions () const
{
  return false;
}

bool
BaseType::has_subsititions_defined () const
{
  return false;
}

bool
BaseType::can_substitute () const
{
  return supports_substitutions () && has_subsititions_defined ();
}

bool
BaseType::needs_generic_substitutions () const
{
  return false;
}

bool
BaseType::contains_type_parameters () const
{
  return !is_concrete ();
}

const RustIdent &
BaseType::get_ident () const
{
  return ident;
}

Location
BaseType::get_locus () const
{
  return ident.locus;
}

// FIXME this is missing locus
bool
BaseType::satisfies_bound (const TypeBoundPredicate &predicate) const
{
  const Resolver::TraitReference *query = predicate.get ();
  for (const auto &bound : specified_bounds)
    {
      const Resolver::TraitReference *item = bound.get ();
      if (item->satisfies_bound (*query))
	return true;
    }

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
	      RichLocation r (mappings->lookup_location (get_ref ()));
	      r.add_range (predicate.get_locus ());
	      r.add_range (mappings->lookup_location (i.get_hirid ()));

	      rust_error_at (
		r, "expected %<%s%> got %<%s%>",
		bound_ty->destructure ()->get_name ().c_str (),
		impl_item_ty->destructure ()->get_name ().c_str ());
	      return false;
	    }
	}

      return true;
    }

  return false;
}

bool
BaseType::bounds_compatible (const BaseType &other, Location locus,
			     bool emit_error) const
{
  std::vector<std::reference_wrapper<const TypeBoundPredicate>>
    unsatisfied_bounds;
  for (auto &bound : get_specified_bounds ())
    {
      if (!other.satisfies_bound (bound))
	unsatisfied_bounds.push_back (bound);
    }

  // lets emit a single error for this
  if (unsatisfied_bounds.size () > 0)
    {
      RichLocation r (locus);
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
	  rust_error_at (r,
			 "bounds not satisfied for %s %<%s%> is not satisfied",
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
  if (get_kind () == TyTy::REF)
    {
      const ReferenceType *r = static_cast<const ReferenceType *> (root);
      root = r->get_base ()->get_root ();
    }
  else if (get_kind () == TyTy::POINTER)
    {
      const PointerType *r = static_cast<const PointerType *> (root);
      root = r->get_base ()->get_root ();
    }

  // these are an unsize
  else if (get_kind () == TyTy::SLICE)
    {
      const SliceType *r = static_cast<const SliceType *> (root);
      root = r->get_element_type ()->get_root ();
    }
  // else if (get_kind () == TyTy::ARRAY)
  //   {
  //     const ArrayType *r = static_cast<const ArrayType *> (root);
  //     root = r->get_element_type ()->get_root ();
  //   }

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
	    Location (),
	    "%<recursion depth%> count exceeds limit of %i (use "
	    "%<frust-max-recursion-depth=%> to increase the limit)",
	    rust_max_recursion_depth);
	  return new ErrorType (get_ref ());
	}

      switch (x->get_kind ())
	{
	  case TyTy::TypeKind::PARAM: {
	    TyTy::ParamType *p = static_cast<TyTy::ParamType *> (x);
	    TyTy::BaseType *pr = p->resolve ();
	    if (pr == x)
	      return pr;

	    x = pr;
	  }
	  break;

	  case TyTy::TypeKind::PLACEHOLDER: {
	    TyTy::PlaceholderType *p = static_cast<TyTy::PlaceholderType *> (x);
	    if (!p->can_resolve ())
	      return p;

	    x = p->resolve ();
	  }
	  break;

	  case TyTy::TypeKind::PROJECTION: {
	    TyTy::ProjectionType *p = static_cast<TyTy::ProjectionType *> (x);
	    x = p->get ();
	  }
	  break;

	default:
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
	    Location (),
	    "%<recursion depth%> count exceeds limit of %i (use "
	    "%<frust-max-recursion-depth=%> to increase the limit)",
	    rust_max_recursion_depth);
	  return new ErrorType (get_ref ());
	}

      switch (x->get_kind ())
	{
	  case TyTy::TypeKind::PARAM: {
	    const TyTy::ParamType *p = static_cast<const TyTy::ParamType *> (x);
	    const TyTy::BaseType *pr = p->resolve ();
	    if (pr == x)
	      return pr;

	    x = pr;
	  }
	  break;

	  case TyTy::TypeKind::PLACEHOLDER: {
	    const TyTy::PlaceholderType *p
	      = static_cast<const TyTy::PlaceholderType *> (x);
	    if (!p->can_resolve ())
	      return p;

	    x = p->resolve ();
	  }
	  break;

	  case TyTy::TypeKind::PROJECTION: {
	    const TyTy::ProjectionType *p
	      = static_cast<const TyTy::ProjectionType *> (x);
	    x = p->get ();
	  }
	  break;

	default:
	  return x;
	}
    }

  return x;
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

// InferType

InferType::InferType (HirId ref, InferTypeKind infer_kind, Location locus,
		      std::set<HirId> refs)
  : BaseType (ref, ref, TypeKind::INFER,
	      {Resolver::CanonicalPath::create_empty (), locus}, refs),
    infer_kind (infer_kind)
{}

InferType::InferType (HirId ref, HirId ty_ref, InferTypeKind infer_kind,
		      Location locus, std::set<HirId> refs)
  : BaseType (ref, ty_ref, TypeKind::INFER,
	      {Resolver::CanonicalPath::create_empty (), locus}, refs),
    infer_kind (infer_kind)
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

bool
InferType::is_concrete () const
{
  return true;
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

  auto mappings = Analysis::Mappings::get ();
  auto context = Resolver::TypeCheckContext::get ();

  InferType *clone
    = new InferType (mappings->get_next_hir_id (), get_infer_kind (),
		     get_ident ().locus, get_combined_refs ());

  context->insert_type (Analysis::NodeMapping (mappings->get_current_crate (),
					       UNKNOWN_NODEID,
					       clone->get_ref (),
					       UNKNOWN_LOCAL_DEFID),
			clone);
  mappings->insert_location (clone->get_ref (),
			     mappings->lookup_location (get_ref ()));

  // setup the chain to reference this
  clone->append_reference (get_ref ());

  return clone;
}

BaseType *
InferType::monomorphized_clone () const
{
  return clone ();
}

bool
InferType::default_type (BaseType **type) const
{
  auto context = Resolver::TypeCheckContext::get ();
  bool ok = false;
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

// ErrorType

ErrorType::ErrorType (HirId ref, std::set<HirId> refs)
  : BaseType (ref, ref, TypeKind::ERROR,
	      {Resolver::CanonicalPath::create_empty (), Location ()}, refs)
{}

ErrorType::ErrorType (HirId ref, HirId ty_ref, std::set<HirId> refs)
  : BaseType (ref, ty_ref, TypeKind::ERROR,
	      {Resolver::CanonicalPath::create_empty (), Location ()}, refs)
{}

bool
ErrorType::is_unit () const
{
  return true;
}
bool
ErrorType::is_concrete () const
{
  return false;
}

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

BaseType *
ErrorType::monomorphized_clone () const
{
  return clone ();
}

// Struct Field type

StructFieldType::StructFieldType (HirId ref, std::string name, BaseType *ty,
				  Location locus)
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

bool
StructFieldType::is_concrete () const
{
  return ty->is_concrete ();
}

void
StructFieldType::debug () const
{
  rust_debug ("%s", as_string ().c_str ());
}

Location
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
  bool names_eq = get_name ().compare (other.get_name ()) == 0;

  TyTy::BaseType *o = other.get_field_type ();
  if (o->get_kind () == TypeKind::PARAM)
    {
      ParamType *op = static_cast<ParamType *> (o);
      o = op->resolve ();
    }

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
  gcc_unreachable ();
  return "";
}

VariantDef::VariantDef (HirId id, DefId defid, std::string identifier,
			RustIdent ident, HIR::Expr *discriminant)
  : id (id), defid (defid), identifier (identifier), ident (ident),
    discriminant (discriminant)

{
  type = VariantType::NUM;
  fields = {};
}

VariantDef::VariantDef (HirId id, DefId defid, std::string identifier,
			RustIdent ident, VariantType type,
			HIR::Expr *discriminant,
			std::vector<StructFieldType *> fields)
  : id (id), defid (defid), identifier (identifier), ident (ident), type (type),
    discriminant (discriminant), fields (fields)
{
  rust_assert ((type == VariantType::NUM && fields.empty ())
	       || (type == VariantType::TUPLE || type == VariantType::STRUCT));
}

VariantDef::VariantDef (const VariantDef &other)
  : id (other.id), defid (other.defid), identifier (other.identifier),
    ident (other.ident), type (other.type), discriminant (other.discriminant),
    fields (other.fields)
{}

VariantDef &
VariantDef::operator= (const VariantDef &other)
{
  id = other.id;
  identifier = other.identifier;
  type = other.type;
  discriminant = other.discriminant;
  fields = other.fields;
  ident = other.ident;

  return *this;
}

VariantDef &
VariantDef::get_error_node ()
{
  static VariantDef node
    = VariantDef (UNKNOWN_HIRID, UNKNOWN_DEFID, "",
		  {Resolver::CanonicalPath::create_empty (),
		   Linemap::unknown_location ()},
		  nullptr);

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

HIR::Expr *
VariantDef::get_discriminant () const
{
  rust_assert (discriminant != nullptr);
  return discriminant;
}

std::string
VariantDef::as_string () const
{
  if (type == VariantType::NUM)
    return identifier + " = " + discriminant->as_string ();

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

  if (discriminant != other.discriminant)
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

  return new VariantDef (id, defid, identifier, ident, type, discriminant,
			 cloned_fields);
}

VariantDef *
VariantDef::monomorphized_clone () const
{
  std::vector<StructFieldType *> cloned_fields;
  for (auto &f : fields)
    cloned_fields.push_back ((StructFieldType *) f->monomorphized_clone ());

  return new VariantDef (id, defid, identifier, ident, type, discriminant,
			 cloned_fields);
}

const RustIdent &
VariantDef::get_ident () const
{
  return ident;
}

// ADTType

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

  auto other2 = static_cast<const ADTType &> (other);
  if (get_adt_kind () != other2.get_adt_kind ())
    return false;

  if (number_of_variants () != other2.number_of_variants ())
    return false;

  if (has_subsititions_defined () != other2.has_subsititions_defined ())
    return false;

  if (has_subsititions_defined ())
    {
      if (get_num_substitutions () != other2.get_num_substitutions ())
	return false;

      for (size_t i = 0; i < get_num_substitutions (); i++)
	{
	  const SubstitutionParamMapping &a = substitutions.at (i);
	  const SubstitutionParamMapping &b = other2.substitutions.at (i);

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
      const TyTy::VariantDef *b = other2.get_variants ().at (i);

      if (!a->is_equal (*b))
	return false;
    }

  return true;
}

BaseType *
ADTType::clone () const
{
  std::vector<VariantDef *> cloned_variants;
  for (auto &variant : variants)
    cloned_variants.push_back (variant->clone ());

  return new ADTType (get_ref (), get_ty_ref (), identifier, ident,
		      get_adt_kind (), cloned_variants, clone_substs (),
		      get_repr_options (), used_arguments,
		      get_combined_refs ());
}

BaseType *
ADTType::monomorphized_clone () const
{
  std::vector<VariantDef *> cloned_variants;
  for (auto &variant : variants)
    cloned_variants.push_back (variant->monomorphized_clone ());

  return new ADTType (get_ref (), get_ty_ref (), identifier, ident,
		      get_adt_kind (), cloned_variants, clone_substs (),
		      get_repr_options (), used_arguments,
		      get_combined_refs ());
}

static bool
handle_substitions (SubstitutionArgumentMappings &subst_mappings,
		    StructFieldType *field)
{
  auto fty = field->get_field_type ();
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
	      field->set_field_type (new_field);
	    }
	  else
	    {
	      field->get_field_type ()->set_ty_ref (argt->get_ref ());
	    }
	}
    }
  else if (fty->has_subsititions_defined () || fty->contains_type_parameters ())
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
  ADTType *adt = static_cast<ADTType *> (clone ());
  adt->set_ty_ref (mappings->get_next_hir_id ());
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

TupleType::TupleType (HirId ref, Location locus, std::vector<TyVar> fields,
		      std::set<HirId> refs)
  : BaseType (ref, ref, TypeKind::TUPLE,
	      {Resolver::CanonicalPath::create_empty (), locus}, refs),
    fields (fields)
{}

TupleType::TupleType (HirId ref, HirId ty_ref, Location locus,
		      std::vector<TyVar> fields, std::set<HirId> refs)
  : BaseType (ref, ty_ref, TypeKind::TUPLE,
	      {Resolver::CanonicalPath::create_empty (), locus}, refs),
    fields (fields)
{}

TupleType *
TupleType::get_unit_type (HirId ref)
{
  return new TupleType (ref, Linemap::predeclared_location ());
}

bool
TupleType::is_unit () const
{
  return this->fields.empty ();
}

size_t
TupleType::num_fields () const
{
  return fields.size ();
}

bool
TupleType::is_concrete () const
{
  for (size_t i = 0; i < num_fields (); i++)
    {
      if (!get_field (i)->is_concrete ())
	return false;
    }
  return true;
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

  auto other2 = static_cast<const TupleType &> (other);
  if (num_fields () != other2.num_fields ())
    return false;

  for (size_t i = 0; i < num_fields (); i++)
    {
      if (!get_field (i)->is_equal (*other2.get_field (i)))
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

BaseType *
TupleType::monomorphized_clone () const
{
  std::vector<TyVar> cloned_fields;
  for (const auto &f : fields)
    cloned_fields.push_back (f.monomorphized_clone ());

  return new TupleType (get_ref (), get_ty_ref (), get_ident ().locus,
			cloned_fields, get_combined_refs ());
}

TupleType *
TupleType::handle_substitions (SubstitutionArgumentMappings &mappings)
{
  auto mappings_table = Analysis::Mappings::get ();

  TupleType *tuple = static_cast<TupleType *> (clone ());
  tuple->set_ref (mappings_table->get_next_hir_id ());
  tuple->set_ty_ref (mappings_table->get_next_hir_id ());

  for (size_t i = 0; i < tuple->fields.size (); i++)
    {
      TyVar &field = fields.at (i);
      if (field.get_tyty ()->contains_type_parameters ())
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
      auto pattern = param.first;
      auto ty = param.second;
      params_str += pattern->as_string () + " " + ty->as_string ();
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

  auto other2 = static_cast<const FnType &> (other);
  if (get_identifier ().compare (other2.get_identifier ()) != 0)
    return false;

  if (!get_return_type ()->is_equal (*other2.get_return_type ()))
    return false;

  if (has_subsititions_defined () != other2.has_subsititions_defined ())
    return false;

  if (has_subsititions_defined ())
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
      auto lhs = param_at (i).second;
      auto rhs = other2.param_at (i).second;
      if (!lhs->is_equal (*rhs))
	return false;
    }
  return true;
}

BaseType *
FnType::clone () const
{
  std::vector<std::pair<HIR::Pattern *, BaseType *>> cloned_params;
  for (auto &p : params)
    cloned_params.push_back ({p.first, p.second->clone ()});

  return new FnType (get_ref (), get_ty_ref (), get_id (), get_identifier (),
		     ident, flags, abi, std::move (cloned_params),
		     get_return_type ()->clone (), clone_substs (),
		     get_combined_refs ());
}

BaseType *
FnType::monomorphized_clone () const
{
  std::vector<std::pair<HIR::Pattern *, BaseType *>> cloned_params;
  for (auto &p : params)
    cloned_params.push_back ({p.first, p.second->monomorphized_clone ()});

  return new FnType (get_ref (), get_ty_ref (), get_id (), get_identifier (),
		     ident, flags, abi, std::move (cloned_params),
		     get_return_type ()->clone (), clone_substs (),
		     get_combined_refs ());
}

FnType *
FnType::handle_substitions (SubstitutionArgumentMappings &subst_mappings)
{
  FnType *fn = static_cast<FnType *> (clone ());
  fn->set_ty_ref (mappings->get_next_hir_id ());
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
  else if (fty->needs_generic_substitutions ()
	   || fty->contains_type_parameters ())
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
      auto fty = param.second;

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
		  param.second = new_field;
		}
	      else
		{
		  fty->set_ty_ref (argt->get_ref ());
		}
	    }
	}
      else if (fty->has_subsititions_defined ()
	       || fty->contains_type_parameters ())
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
	  param.second = new_field;
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
      if (!param_at (i)->is_equal (*other2.param_at (i)))
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

BaseType *
FnPtr::monomorphized_clone () const
{
  std::vector<TyVar> cloned_params;
  for (auto &p : params)
    cloned_params.push_back (p.monomorphized_clone ());

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

BaseType *
ClosureType::monomorphized_clone () const
{
  return clone ();
}

ClosureType *
ClosureType::handle_substitions (SubstitutionArgumentMappings &mappings)
{
  gcc_unreachable ();
  return nullptr;
}

void
ClosureType::setup_fn_once_output () const
{
  // lookup the lang items
  auto fn_once_lang_item = Analysis::RustLangItem::ItemType::FN_ONCE;
  auto fn_once_output_lang_item
    = Analysis::RustLangItem::ItemType::FN_ONCE_OUTPUT;

  DefId trait_id = UNKNOWN_DEFID;
  bool trait_lang_item_defined
    = mappings->lookup_lang_item (fn_once_lang_item, &trait_id);
  rust_assert (trait_lang_item_defined);

  DefId trait_item_id = UNKNOWN_DEFID;
  bool trait_item_lang_item_defined
    = mappings->lookup_lang_item (fn_once_output_lang_item, &trait_item_id);
  rust_assert (trait_item_lang_item_defined);

  // resolve to the trait
  HIR::Item *item = mappings->lookup_defid (trait_id);
  rust_assert (item->get_item_kind () == HIR::Item::ItemKind::Trait);
  HIR::Trait *trait = static_cast<HIR::Trait *> (item);

  Resolver::TraitReference *trait_ref
    = Resolver::TraitResolver::Resolve (*trait);
  rust_assert (!trait_ref->is_error ());

  // resolve to trait item
  HIR::TraitItem *trait_item
    = mappings->lookup_trait_item_defid (trait_item_id);
  rust_assert (trait_item != nullptr);
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

BaseType *
ArrayType::clone () const
{
  return new ArrayType (get_ref (), get_ty_ref (), ident.locus, capacity_expr,
			element_type, get_combined_refs ());
}

BaseType *
ArrayType::monomorphized_clone () const
{
  return new ArrayType (get_ref (), get_ty_ref (), ident.locus, capacity_expr,
			element_type.monomorphized_clone (),
			get_combined_refs ());
}

ArrayType *
ArrayType::handle_substitions (SubstitutionArgumentMappings &mappings)
{
  auto mappings_table = Analysis::Mappings::get ();

  ArrayType *ref = static_cast<ArrayType *> (clone ());
  ref->set_ty_ref (mappings_table->get_next_hir_id ());

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

BaseType *
SliceType::clone () const
{
  return new SliceType (get_ref (), get_ty_ref (), ident.locus,
			element_type.clone (), get_combined_refs ());
}

BaseType *
SliceType::monomorphized_clone () const
{
  return new SliceType (get_ref (), get_ty_ref (), ident.locus,
			element_type.monomorphized_clone (),
			get_combined_refs ());
}

SliceType *
SliceType::handle_substitions (SubstitutionArgumentMappings &mappings)
{
  auto mappings_table = Analysis::Mappings::get ();

  SliceType *ref = static_cast<SliceType *> (clone ());
  ref->set_ty_ref (mappings_table->get_next_hir_id ());

  // might be &T or &ADT so this needs to be recursive
  auto base = ref->get_element_type ();
  BaseType *concrete = Resolver::SubstMapperInternal::Resolve (base, mappings);
  ref->element_type = TyVar::subst_covariant_var (base, concrete);

  return ref;
}

// BoolType

BoolType::BoolType (HirId ref, std::set<HirId> refs)
  : BaseType (ref, ref, TypeKind::BOOL,
	      {Resolver::CanonicalPath::create_empty (),
	       Linemap::predeclared_location ()},
	      refs)
{}

BoolType::BoolType (HirId ref, HirId ty_ref, std::set<HirId> refs)
  : BaseType (ref, ty_ref, TypeKind::BOOL,
	      {Resolver::CanonicalPath::create_empty (),
	       Linemap::predeclared_location ()},
	      refs)
{}

std::string
BoolType::get_name () const
{
  return as_string ();
}

bool
BoolType::is_concrete () const
{
  return true;
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

BaseType *
BoolType::monomorphized_clone () const
{
  return clone ();
}

// IntType

IntType::IntType (HirId ref, IntKind kind, std::set<HirId> refs)
  : BaseType (ref, ref, TypeKind::INT,
	      {Resolver::CanonicalPath::create_empty (),
	       Linemap::predeclared_location ()},
	      refs),
    int_kind (kind)
{}

IntType::IntType (HirId ref, HirId ty_ref, IntKind kind, std::set<HirId> refs)
  : BaseType (ref, ty_ref, TypeKind::INT,
	      {Resolver::CanonicalPath::create_empty (),
	       Linemap::predeclared_location ()},
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
  gcc_unreachable ();
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

BaseType *
IntType::monomorphized_clone () const
{
  return clone ();
}

bool
IntType::is_equal (const BaseType &other) const
{
  if (!BaseType::is_equal (other))
    return false;

  const IntType &o = static_cast<const IntType &> (other);
  return get_int_kind () == o.get_int_kind ();
}

bool
IntType::is_concrete () const
{
  return true;
}

// UintType

UintType::UintType (HirId ref, UintKind kind, std::set<HirId> refs)
  : BaseType (ref, ref, TypeKind::UINT,
	      {Resolver::CanonicalPath::create_empty (),
	       Linemap::predeclared_location ()},
	      refs),
    uint_kind (kind)
{}

UintType::UintType (HirId ref, HirId ty_ref, UintKind kind,
		    std::set<HirId> refs)
  : BaseType (ref, ty_ref, TypeKind::UINT,
	      {Resolver::CanonicalPath::create_empty (),
	       Linemap::predeclared_location ()},
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
  gcc_unreachable ();
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

BaseType *
UintType::monomorphized_clone () const
{
  return clone ();
}

bool
UintType::is_equal (const BaseType &other) const
{
  if (!BaseType::is_equal (other))
    return false;

  const UintType &o = static_cast<const UintType &> (other);
  return get_uint_kind () == o.get_uint_kind ();
}

bool
UintType::is_concrete () const
{
  return true;
}

// FloatType

FloatType::FloatType (HirId ref, FloatKind kind, std::set<HirId> refs)
  : BaseType (ref, ref, TypeKind::FLOAT,
	      {Resolver::CanonicalPath::create_empty (),
	       Linemap::predeclared_location ()},
	      refs),
    float_kind (kind)
{}

FloatType::FloatType (HirId ref, HirId ty_ref, FloatKind kind,
		      std::set<HirId> refs)
  : BaseType (ref, ty_ref, TypeKind::FLOAT,
	      {Resolver::CanonicalPath::create_empty (),
	       Linemap::predeclared_location ()},
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

bool
FloatType::is_concrete () const
{
  return true;
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
  gcc_unreachable ();
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

BaseType *
FloatType::monomorphized_clone () const
{
  return clone ();
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
  : BaseType (ref, ref, TypeKind::USIZE,
	      {Resolver::CanonicalPath::create_empty (),
	       Linemap::predeclared_location ()},
	      refs)
{}

USizeType::USizeType (HirId ref, HirId ty_ref, std::set<HirId> refs)
  : BaseType (ref, ty_ref, TypeKind::USIZE,
	      {Resolver::CanonicalPath::create_empty (),
	       Linemap::predeclared_location ()},
	      refs)
{}

std::string
USizeType::get_name () const
{
  return as_string ();
}

bool
USizeType::is_concrete () const
{
  return true;
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

BaseType *
USizeType::monomorphized_clone () const
{
  return clone ();
}

// ISizeType

ISizeType::ISizeType (HirId ref, std::set<HirId> refs)
  : BaseType (ref, ref, TypeKind::ISIZE,
	      {Resolver::CanonicalPath::create_empty (),
	       Linemap::predeclared_location ()},
	      refs)
{}

ISizeType::ISizeType (HirId ref, HirId ty_ref, std::set<HirId> refs)
  : BaseType (ref, ty_ref, TypeKind::ISIZE,
	      {Resolver::CanonicalPath::create_empty (),
	       Linemap::predeclared_location ()},
	      refs)
{}

std::string
ISizeType::get_name () const
{
  return as_string ();
}

bool
ISizeType::is_concrete () const
{
  return true;
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

BaseType *
ISizeType::monomorphized_clone () const
{
  return clone ();
}

// Char Type

CharType::CharType (HirId ref, std::set<HirId> refs)
  : BaseType (ref, ref, TypeKind::CHAR,
	      {Resolver::CanonicalPath::create_empty (),
	       Linemap::predeclared_location ()},
	      refs)
{}

CharType::CharType (HirId ref, HirId ty_ref, std::set<HirId> refs)
  : BaseType (ref, ty_ref, TypeKind::CHAR,
	      {Resolver::CanonicalPath::create_empty (),
	       Linemap::predeclared_location ()},
	      refs)
{}

bool
CharType::is_concrete () const
{
  return true;
}

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

BaseType *
CharType::monomorphized_clone () const
{
  return clone ();
}

// Reference Type

ReferenceType::ReferenceType (HirId ref, TyVar base, Mutability mut,
			      std::set<HirId> refs)
  : BaseType (ref, ref, TypeKind::REF,
	      {Resolver::CanonicalPath::create_empty (),
	       Linemap::predeclared_location ()},
	      refs),
    base (base), mut (mut)
{}

ReferenceType::ReferenceType (HirId ref, HirId ty_ref, TyVar base,
			      Mutability mut, std::set<HirId> refs)
  : BaseType (ref, ty_ref, TypeKind::REF,
	      {Resolver::CanonicalPath::create_empty (),
	       Linemap::predeclared_location ()},
	      refs),
    base (base), mut (mut)
{}

bool
ReferenceType::is_concrete () const
{
  return get_base ()->is_concrete ();
}

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

bool
ReferenceType::is_dyn_object () const
{
  return is_dyn_slice_type () || is_dyn_str_type ();
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

BaseType *
ReferenceType::clone () const
{
  return new ReferenceType (get_ref (), get_ty_ref (), base, mutability (),
			    get_combined_refs ());
}

BaseType *
ReferenceType::monomorphized_clone () const
{
  return new ReferenceType (get_ref (), get_ty_ref (),
			    base.monomorphized_clone (), mutability (),
			    get_combined_refs ());
}

ReferenceType *
ReferenceType::handle_substitions (SubstitutionArgumentMappings &mappings)
{
  auto mappings_table = Analysis::Mappings::get ();

  ReferenceType *ref = static_cast<ReferenceType *> (clone ());
  ref->set_ty_ref (mappings_table->get_next_hir_id ());

  // might be &T or &ADT so this needs to be recursive
  auto base = ref->get_base ();
  BaseType *concrete = Resolver::SubstMapperInternal::Resolve (base, mappings);
  ref->base = TyVar::subst_covariant_var (base, concrete);

  return ref;
}

// PointerType

PointerType::PointerType (HirId ref, TyVar base, Mutability mut,
			  std::set<HirId> refs)
  : BaseType (ref, ref, TypeKind::POINTER,
	      {Resolver::CanonicalPath::create_empty (),
	       Linemap::predeclared_location ()},
	      refs),
    base (base), mut (mut)
{}

PointerType::PointerType (HirId ref, HirId ty_ref, TyVar base, Mutability mut,
			  std::set<HirId> refs)
  : BaseType (ref, ty_ref, TypeKind::POINTER,
	      {Resolver::CanonicalPath::create_empty (),
	       Linemap::predeclared_location ()},
	      refs),
    base (base), mut (mut)
{}

bool
PointerType::is_concrete () const
{
  return get_base ()->is_concrete ();
}

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
  return is_dyn_slice_type () || is_dyn_str_type ();
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

BaseType *
PointerType::clone () const
{
  return new PointerType (get_ref (), get_ty_ref (), base, mutability (),
			  get_combined_refs ());
}

BaseType *
PointerType::monomorphized_clone () const
{
  return new PointerType (get_ref (), get_ty_ref (),
			  base.monomorphized_clone (), mutability (),
			  get_combined_refs ());
}

PointerType *
PointerType::handle_substitions (SubstitutionArgumentMappings &mappings)
{
  auto mappings_table = Analysis::Mappings::get ();

  PointerType *ref = static_cast<PointerType *> (clone ());
  ref->set_ty_ref (mappings_table->get_next_hir_id ());

  // might be &T or &ADT so this needs to be recursive
  auto base = ref->get_base ();
  BaseType *concrete = Resolver::SubstMapperInternal::Resolve (base, mappings);
  ref->base = TyVar::subst_covariant_var (base, concrete);

  return ref;
}

// PARAM Type

ParamType::ParamType (std::string symbol, Location locus, HirId ref,
		      HIR::GenericParam &param,
		      std::vector<TypeBoundPredicate> specified_bounds,
		      std::set<HirId> refs)
  : BaseType (ref, ref, TypeKind::PARAM,
	      {Resolver::CanonicalPath::new_seg (UNKNOWN_NODEID, symbol),
	       locus},
	      specified_bounds, refs),
    is_trait_self (false), symbol (symbol), param (param)
{}

ParamType::ParamType (bool is_trait_self, std::string symbol, Location locus,
		      HirId ref, HirId ty_ref, HIR::GenericParam &param,
		      std::vector<TypeBoundPredicate> specified_bounds,
		      std::set<HirId> refs)
  : BaseType (ref, ty_ref, TypeKind::PARAM,
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

bool
ParamType::is_concrete () const
{
  auto r = resolve ();
  if (r == this)
    return false;

  return r->is_concrete ();
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

  return resolve ()->get_name ();
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

BaseType *
ParamType::monomorphized_clone () const
{
  return resolve ()->clone ();
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
  p->set_ref (mappings->get_next_hir_id ());
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

// StrType

StrType::StrType (HirId ref, std::set<HirId> refs)
  : BaseType (ref, ref, TypeKind::STR,
	      {Resolver::CanonicalPath::create_empty (),
	       Linemap::predeclared_location ()},
	      refs)
{}

StrType::StrType (HirId ref, HirId ty_ref, std::set<HirId> refs)
  : BaseType (ref, ty_ref, TypeKind::STR,
	      {Resolver::CanonicalPath::create_empty (),
	       Linemap::predeclared_location ()},
	      refs)
{}

std::string
StrType::get_name () const
{
  return as_string ();
}

bool
StrType::is_concrete () const
{
  return true;
}

BaseType *
StrType::clone () const
{
  return new StrType (get_ref (), get_ty_ref (), get_combined_refs ());
}

BaseType *
StrType::monomorphized_clone () const
{
  return clone ();
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
  : BaseType (ref, ref, TypeKind::NEVER,
	      {Resolver::CanonicalPath::create_empty (),
	       Linemap::predeclared_location ()},
	      refs)
{}

NeverType::NeverType (HirId ref, HirId ty_ref, std::set<HirId> refs)
  : BaseType (ref, ty_ref, TypeKind::NEVER,
	      {Resolver::CanonicalPath::create_empty (),
	       Linemap::predeclared_location ()},
	      refs)
{}

std::string
NeverType::get_name () const
{
  return as_string ();
}

bool
NeverType::is_unit () const
{
  return true;
}

bool
NeverType::is_concrete () const
{
  return true;
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

BaseType *
NeverType::monomorphized_clone () const
{
  return clone ();
}

// placeholder type

PlaceholderType::PlaceholderType (std::string symbol, HirId ref,
				  std::set<HirId> refs)
  : BaseType (ref, ref, TypeKind::PLACEHOLDER,
	      {Resolver::CanonicalPath::create_empty (),
	       Linemap::predeclared_location ()},
	      refs),
    symbol (symbol)
{}

PlaceholderType::PlaceholderType (std::string symbol, HirId ref, HirId ty_ref,
				  std::set<HirId> refs)
  : BaseType (ref, ty_ref, TypeKind::PLACEHOLDER,
	      {Resolver::CanonicalPath::create_empty (),
	       Linemap::predeclared_location ()},
	      refs),
    symbol (symbol)
{}

std::string
PlaceholderType::get_name () const
{
  return as_string ();
}

bool
PlaceholderType::is_unit () const
{
  rust_assert (can_resolve ());
  return resolve ()->is_unit ();
}

std::string
PlaceholderType::get_symbol () const
{
  return symbol;
}

bool
PlaceholderType::is_concrete () const
{
  if (!can_resolve ())
    return true;

  return resolve ()->is_concrete ();
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
  return new PlaceholderType (get_symbol (), get_ref (), get_ty_ref (),
			      get_combined_refs ());
}

BaseType *
PlaceholderType::monomorphized_clone () const
{
  if (can_resolve ())
    return resolve ()->monomorphized_clone ();

  return clone ();
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
  return context->lookup_associated_type_mapping (get_ty_ref (), nullptr);
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

// Projection type

ProjectionType::ProjectionType (
  HirId ref, BaseType *base, const Resolver::TraitReference *trait, DefId item,
  std::vector<SubstitutionParamMapping> subst_refs,
  SubstitutionArgumentMappings generic_arguments, std::set<HirId> refs)
  : BaseType (ref, ref, TypeKind::PROJECTION,
	      {Resolver::CanonicalPath::create_empty (),
	       Linemap::predeclared_location ()},
	      refs),
    SubstitutionRef (std::move (subst_refs), std::move (generic_arguments)),
    base (base), trait (trait), item (item)
{}

ProjectionType::ProjectionType (
  HirId ref, HirId ty_ref, BaseType *base,
  const Resolver::TraitReference *trait, DefId item,
  std::vector<SubstitutionParamMapping> subst_refs,
  SubstitutionArgumentMappings generic_arguments, std::set<HirId> refs)
  : BaseType (ref, ty_ref, TypeKind::PROJECTION,
	      {Resolver::CanonicalPath::create_empty (),
	       Linemap::predeclared_location ()},
	      refs),
    SubstitutionRef (std::move (subst_refs), std::move (generic_arguments)),
    base (base), trait (trait), item (item)
{}

bool
ProjectionType::is_unit () const
{
  return false;
}

std::string
ProjectionType::get_name () const
{
  return as_string ();
}

bool
ProjectionType::needs_generic_substitutions () const
{
  return needs_substitution ();
}

bool
ProjectionType::supports_substitutions () const
{
  return true;
}

bool
ProjectionType::has_subsititions_defined () const
{
  return has_substitutions ();
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

bool
ProjectionType::is_concrete () const
{
  return base->is_concrete ();
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
			     get_combined_refs ());
}

BaseType *
ProjectionType::monomorphized_clone () const
{
  return get ()->monomorphized_clone ();
}

ProjectionType *
ProjectionType::handle_substitions (
  SubstitutionArgumentMappings &subst_mappings)
{
  // // do we really need to substitute this?
  // if (base->needs_generic_substitutions () || base->contains_type_parameters
  // ())
  //   {
  //     return this;
  //   }

  ProjectionType *projection = static_cast<ProjectionType *> (clone ());
  projection->set_ty_ref (mappings->get_next_hir_id ());
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
  else if (fty->needs_generic_substitutions ()
	   || fty->contains_type_parameters ())
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
  : BaseType (ref, ref, TypeKind::DYNAMIC, ident, specified_bounds, refs)
{}

DynamicObjectType::DynamicObjectType (
  HirId ref, HirId ty_ref, RustIdent ident,
  std::vector<TypeBoundPredicate> specified_bounds, std::set<HirId> refs)
  : BaseType (ref, ty_ref, TypeKind::DYNAMIC, ident, specified_bounds, refs)
{}

bool
DynamicObjectType::is_concrete () const
{
  return true;
}

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

BaseType *
DynamicObjectType::monomorphized_clone () const
{
  return clone ();
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

  return bounds_compatible (other, Location (), false);
}

const std::vector<
  std::pair<const Resolver::TraitItemReference *, const TypeBoundPredicate *>>
DynamicObjectType::get_object_items () const
{
  std::vector<
    std::pair<const Resolver::TraitItemReference *, const TypeBoundPredicate *>>
    items;
  for (auto &bound : get_specified_bounds ())
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
