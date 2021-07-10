// Copyright (C) 2020 Free Software Foundation, Inc.

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
#include "rust-tyty-visitor.h"
#include "rust-tyty-call.h"
#include "rust-hir-type-check-expr.h"
#include "rust-hir-type-check-type.h"
#include "rust-tyty-rules.h"
#include "rust-tyty-cmp.h"
#include "rust-hir-map.h"
#include "rust-substitution-mapper.h"

extern ::Backend *
rust_get_backend ();

namespace Rust {
namespace TyTy {

TyVar::TyVar (HirId ref) : ref (ref)
{
  // ensure this reference is defined within the context
  auto context = Resolver::TypeCheckContext::get ();
  BaseType *lookup = nullptr;
  bool ok = context->lookup_type (ref, &lookup);
  rust_assert (ok);
}

BaseType *
TyVar::get_tyty () const
{
  auto context = Resolver::TypeCheckContext::get ();
  BaseType *lookup = nullptr;
  bool ok = context->lookup_type (ref, &lookup);
  rust_assert (ok);
  return lookup;
}

TyVar
TyVar::get_implicit_infer_var (Location locus)
{
  auto mappings = Analysis::Mappings::get ();
  auto context = Resolver::TypeCheckContext::get ();

  InferType *infer = new InferType (mappings->get_next_hir_id (),
				    InferType::InferTypeKind::GENERAL);
  context->insert_type (Analysis::NodeMapping (mappings->get_current_crate (),
					       UNKNOWN_NODEID,
					       infer->get_ref (),
					       UNKNOWN_LOCAL_DEFID),
			infer);
  mappings->insert_location (mappings->get_current_crate (), infer->get_ref (),
			     locus);
  return TyVar (infer->get_ref ());
}

void
InferType::accept_vis (TyVisitor &vis)
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

BaseType *
InferType::unify (BaseType *other)
{
  InferRules r (this);
  return r.unify (other);
}

bool
InferType::can_eq (BaseType *other, bool emit_errors)
{
  InferCmp r (this, emit_errors);
  return r.can_eq (other);
}

BaseType *
InferType::clone ()
{
  return new InferType (get_ref (), get_ty_ref (), get_infer_kind (),
			get_combined_refs ());
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

void
ErrorType::accept_vis (TyVisitor &vis)
{
  vis.visit (*this);
}

std::string
ErrorType::as_string () const
{
  return "<tyty::error>";
}

BaseType *
ErrorType::unify (BaseType *other)
{
  return this;
}

bool
ErrorType::can_eq (BaseType *other, bool emit_errors)
{
  return get_kind () == other->get_kind ();
}

BaseType *
ErrorType::clone ()
{
  return new ErrorType (get_ref (), get_ty_ref (), get_combined_refs ());
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

  TyTy::BaseType &o = *other.get_field_type ();
  if (o.get_kind () == TypeKind::PARAM)
    {
      ParamType &op = static_cast<ParamType &> (o);
      o = *op.resolve ();
    }

  bool types_eq = get_field_type ()->is_equal (o);

  return names_eq && types_eq;
}

StructFieldType *
StructFieldType::clone () const
{
  return new StructFieldType (get_ref (), get_name (),
			      get_field_type ()->clone ());
}

void
SubstitutionParamMapping::override_context ()
{
  rust_assert (param->can_resolve ());

  auto mappings = Analysis::Mappings::get ();
  auto context = Resolver::TypeCheckContext::get ();
  context->insert_type (Analysis::NodeMapping (mappings->get_current_crate (),
					       UNKNOWN_NODEID,
					       param->get_ref (),
					       UNKNOWN_LOCAL_DEFID),
			param->resolve ());
}

SubstitutionArgumentMappings
SubstitutionRef::get_mappings_from_generic_args (HIR::GenericArgs &args)
{
  if (args.get_binding_args ().size () > 0)
    {
      RichLocation r (args.get_locus ());
      for (auto &binding : args.get_binding_args ())
	r.add_range (binding.get_locus ());

      rust_error_at (r, "associated type bindings are not allowed here");
      return SubstitutionArgumentMappings::error ();
    }

  if (args.get_type_args ().size () > substitutions.size ())
    {
      RichLocation r (args.get_locus ());
      r.add_range (substitutions.front ().get_param_locus ());

      rust_error_at (
	r,
	"generic item takes at most %lu type arguments but %lu were supplied",
	substitutions.size (), args.get_type_args ().size ());
      return SubstitutionArgumentMappings::error ();
    }

  if (args.get_type_args ().size () < min_required_substitutions ())
    {
      RichLocation r (args.get_locus ());
      r.add_range (substitutions.front ().get_param_locus ());

      rust_error_at (
	r,
	"generic item takes at least %lu type arguments but %lu were supplied",
	substitutions.size (), args.get_type_args ().size ());
      return SubstitutionArgumentMappings::error ();
    }

  // for inherited arguments
  size_t offs = used_arguments.size ();

  std::vector<SubstitutionArg> mappings;
  for (auto &arg : args.get_type_args ())
    {
      BaseType *resolved = Resolver::TypeCheckType::Resolve (arg.get ());
      if (resolved == nullptr || resolved->get_kind () == TyTy::TypeKind::ERROR)
	{
	  rust_error_at (args.get_locus (), "failed to resolve type arguments");
	  return SubstitutionArgumentMappings::error ();
	}

      SubstitutionArg subst_arg (&substitutions.at (offs), resolved);
      offs++;
      mappings.push_back (std::move (subst_arg));
    }

  // we must need to fill out defaults
  size_t left_over
    = num_required_substitutions () - min_required_substitutions ();
  if (left_over > 0)
    {
      for (size_t offs = mappings.size (); offs < substitutions.size (); offs++)
	{
	  SubstitutionParamMapping &param = substitutions.at (offs);
	  rust_assert (param.param_has_default_ty ());

	  BaseType *resolved = param.get_default_ty ();
	  if (resolved->get_kind () == TypeKind::ERROR)
	    return SubstitutionArgumentMappings::error ();

	  // this resolved default might already contain default parameters
	  if (resolved->contains_type_parameters ())
	    {
	      SubstitutionArgumentMappings intermediate (mappings,
							 args.get_locus ());
	      resolved = Resolver::SubstMapperInternal::Resolve (resolved,
								 intermediate);
	      if (resolved->get_kind () == TypeKind::ERROR)
		return SubstitutionArgumentMappings::error ();
	    }

	  SubstitutionArg subst_arg (&param, resolved);
	  mappings.push_back (std::move (subst_arg));
	}
    }

  return SubstitutionArgumentMappings (mappings, args.get_locus ());
}

SubstitutionArgumentMappings
SubstitutionRef::adjust_mappings_for_this (
  SubstitutionArgumentMappings &mappings)
{
  Analysis::Mappings *mappings_table = Analysis::Mappings::get ();

  std::vector<SubstitutionArg> resolved_mappings;
  for (size_t i = 0; i < substitutions.size (); i++)
    {
      auto &subst = substitutions.at (i);

      SubstitutionArg arg = SubstitutionArg::error ();
      if (mappings.size () == substitutions.size ())
	{
	  mappings.get_argument_at (i, &arg);
	}
      else
	{
	  if (subst.needs_substitution ())
	    {
	      // get from passed in mappings
	      mappings.get_argument_for_symbol (subst.get_param_ty (), &arg);
	    }
	  else
	    {
	      // we should already have this somewhere
	      used_arguments.get_argument_for_symbol (subst.get_param_ty (),
						      &arg);
	    }
	}

      bool ok = !arg.is_error ();
      if (!ok)
	{
	  rust_error_at (mappings_table->lookup_location (
			   subst.get_param_ty ()->get_ref ()),
			 "failed to find parameter type: %s vs mappings [%s]",
			 subst.get_param_ty ()->as_string ().c_str (),
			 mappings.as_string ().c_str ());
	  return SubstitutionArgumentMappings::error ();
	}

      SubstitutionArg adjusted (&subst, arg.get_tyty ());
      resolved_mappings.push_back (std::move (adjusted));
    }

  return SubstitutionArgumentMappings (resolved_mappings,
				       mappings.get_locus ());
}

// this function assumes that the mappings being passed are for the same type as
// this new substitution reference so ordering matters here
SubstitutionArgumentMappings
SubstitutionRef::solve_mappings_from_receiver_for_self (
  SubstitutionArgumentMappings &mappings)
{
  std::vector<SubstitutionArg> resolved_mappings;

  rust_assert (mappings.size () == get_num_substitutions ());
  for (size_t i = 0; i < get_num_substitutions (); i++)
    {
      SubstitutionParamMapping &param_mapping = substitutions.at (i);
      SubstitutionArg &arg = mappings.get_mappings ().at (i);

      if (param_mapping.needs_substitution ())
	{
	  SubstitutionArg adjusted (&param_mapping, arg.get_tyty ());
	  resolved_mappings.push_back (std::move (adjusted));
	}
    }

  return SubstitutionArgumentMappings (resolved_mappings,
				       mappings.get_locus ());
}

void
ADTType::accept_vis (TyVisitor &vis)
{
  vis.visit (*this);
}

std::string
ADTType::as_string () const
{
  if (num_fields () == 0)
    return identifier;

  std::string fields_buffer;
  for (size_t i = 0; i < num_fields (); ++i)
    {
      fields_buffer += get_field (i)->as_string ();
      if ((i + 1) < num_fields ())
	fields_buffer += ", ";
    }

  return identifier + subst_as_string () + "{" + fields_buffer + "}";
}

const StructFieldType *
ADTType::get_field (size_t index) const
{
  return fields.at (index);
}

const BaseType *
ADTType::get_field_type (size_t index) const
{
  const StructFieldType *ref = get_field (index);
  auto context = Resolver::TypeCheckContext::get ();
  BaseType *lookup = nullptr;
  bool ok = context->lookup_type (ref->get_field_type ()->get_ref (), &lookup);
  rust_assert (ok);
  return lookup;
}

BaseType *
ADTType::unify (BaseType *other)
{
  ADTRules r (this);
  return r.unify (other);
}

bool
ADTType::can_eq (BaseType *other, bool emit_errors)
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
  if (num_fields () != other2.num_fields ())
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
  else
    {
      for (size_t i = 0; i < num_fields (); i++)
	{
	  if (!get_field (i)->is_equal (*other2.get_field (i)))
	    return false;
	}
    }

  return true;
}

BaseType *
ADTType::clone ()
{
  std::vector<StructFieldType *> cloned_fields;
  for (auto &f : fields)
    cloned_fields.push_back ((StructFieldType *) f->clone ());

  return new ADTType (get_ref (), get_ty_ref (), identifier, get_is_tuple (),
		      cloned_fields, clone_substs (), used_arguments,
		      get_combined_refs ());
}

ADTType *
ADTType::handle_substitions (SubstitutionArgumentMappings subst_mappings)
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
	sub.fill_param_ty (arg.get_tyty ());
    }

  adt->iterate_fields ([&] (StructFieldType *field) mutable -> bool {
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
    else if (fty->has_subsititions_defined ()
	     || fty->contains_type_parameters ())
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
  });

  return adt;
}

void
TupleType::accept_vis (TyVisitor &vis)
{
  vis.visit (*this);
}

std::string
TupleType::as_string () const
{
  std::string fields_buffer;
  iterate_fields ([&] (BaseType *field) mutable -> bool {
    fields_buffer += field->as_string ();
    fields_buffer += ", ";
    return true;
  });
  return "(" + fields_buffer + ")";
}

BaseType *
TupleType::get_field (size_t index) const
{
  return fields.at (index).get_tyty ();
}

BaseType *
TupleType::unify (BaseType *other)
{
  TupleRules r (this);
  return r.unify (other);
}

bool
TupleType::can_eq (BaseType *other, bool emit_errors)
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
TupleType::clone ()
{
  return new TupleType (get_ref (), get_ty_ref (), fields,
			get_combined_refs ());
}

TupleType *
TupleType::handle_substitions (SubstitutionArgumentMappings mappings)
{
  auto mappings_table = Analysis::Mappings::get ();

  TupleType *tuple = static_cast<TupleType *> (clone ());
  tuple->set_ty_ref (mappings_table->get_next_hir_id ());

  for (size_t i = 0; i < tuple->fields.size (); i++)
    {
      TyVar &field = fields.at (i);
      if (field.get_tyty ()->contains_type_parameters ())
	{
	  BaseType *concrete
	    = Resolver::SubstMapperInternal::Resolve (field.get_tyty (),
						      mappings);
	  tuple->fields[i] = TyVar (concrete->get_ty_ref ());
	}
    }

  return tuple;
}

void
FnType::accept_vis (TyVisitor &vis)
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

BaseType *
FnType::unify (BaseType *other)
{
  FnRules r (this);
  return r.unify (other);
}

bool
FnType::can_eq (BaseType *other, bool emit_errors)
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
FnType::clone ()
{
  std::vector<std::pair<HIR::Pattern *, BaseType *> > cloned_params;
  for (auto &p : params)
    cloned_params.push_back (
      std::pair<HIR::Pattern *, BaseType *> (p.first, p.second->clone ()));

  return new FnType (get_ref (), get_ty_ref (), get_identifier (),
		     is_method_flag, std::move (cloned_params),
		     get_return_type ()->clone (), clone_substs (),
		     get_combined_refs ());
}

FnType *
FnType::handle_substitions (SubstitutionArgumentMappings subst_mappings)
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
	sub.fill_param_ty (arg.get_tyty ());
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

std::string
FnPtr::as_string () const
{
  std::string params_str;
  iterate_params ([&] (BaseType *p) mutable -> bool {
    params_str += p->as_string () + " ,";
    return true;
  });
  return "fnptr (" + params_str + ") -> " + get_return_type ()->as_string ();
}

BaseType *
FnPtr::unify (BaseType *other)
{
  FnptrRules r (this);
  return r.unify (other);
}

bool
FnPtr::can_eq (BaseType *other, bool emit_errors)
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
FnPtr::clone ()
{
  std::vector<TyVar> cloned_params;
  for (auto &p : params)
    cloned_params.push_back (TyVar (p.get_ref ()));

  return new FnPtr (get_ref (), get_ty_ref (), std::move (cloned_params),
		    result_type, get_combined_refs ());
}

void
ArrayType::accept_vis (TyVisitor &vis)
{
  vis.visit (*this);
}

std::string
ArrayType::as_string () const
{
  return "[" + get_element_type ()->as_string () + ":" + capacity_string ()
	 + "]";
}

std::string
ArrayType::capacity_string () const
{
  return rust_get_backend ()->const_size_val_to_string (get_capacity ());
}

BaseType *
ArrayType::unify (BaseType *other)
{
  ArrayRules r (this);
  return r.unify (other);
}

bool
ArrayType::can_eq (BaseType *other, bool emit_errors)
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
  if (get_capacity () != other2.get_capacity ())
    return false;

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
ArrayType::clone ()
{
  return new ArrayType (get_ref (), get_ty_ref (), get_capacity (),
			element_type, get_combined_refs ());
}

void
BoolType::accept_vis (TyVisitor &vis)
{
  vis.visit (*this);
}

std::string
BoolType::as_string () const
{
  return "bool";
}

BaseType *
BoolType::unify (BaseType *other)
{
  BoolRules r (this);
  return r.unify (other);
}

bool
BoolType::can_eq (BaseType *other, bool emit_errors)
{
  BoolCmp r (this, emit_errors);
  return r.can_eq (other);
}

BaseType *
BoolType::clone ()
{
  return new BoolType (get_ref (), get_ty_ref (), get_combined_refs ());
}

void
IntType::accept_vis (TyVisitor &vis)
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

BaseType *
IntType::unify (BaseType *other)
{
  IntRules r (this);
  return r.unify (other);
}

bool
IntType::can_eq (BaseType *other, bool emit_errors)
{
  IntCmp r (this, emit_errors);
  return r.can_eq (other);
}

BaseType *
IntType::clone ()
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

void
UintType::accept_vis (TyVisitor &vis)
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

BaseType *
UintType::unify (BaseType *other)
{
  UintRules r (this);
  return r.unify (other);
}

bool
UintType::can_eq (BaseType *other, bool emit_errors)
{
  UintCmp r (this, emit_errors);
  return r.can_eq (other);
}

BaseType *
UintType::clone ()
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

void
FloatType::accept_vis (TyVisitor &vis)
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

BaseType *
FloatType::unify (BaseType *other)
{
  FloatRules r (this);
  return r.unify (other);
}

bool
FloatType::can_eq (BaseType *other, bool emit_errors)
{
  FloatCmp r (this, emit_errors);
  return r.can_eq (other);
}

BaseType *
FloatType::clone ()
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

void
USizeType::accept_vis (TyVisitor &vis)
{
  vis.visit (*this);
}

std::string
USizeType::as_string () const
{
  return "usize";
}

BaseType *
USizeType::unify (BaseType *other)
{
  USizeRules r (this);
  return r.unify (other);
}

bool
USizeType::can_eq (BaseType *other, bool emit_errors)
{
  USizeCmp r (this, emit_errors);
  return r.can_eq (other);
}

BaseType *
USizeType::clone ()
{
  return new USizeType (get_ref (), get_ty_ref (), get_combined_refs ());
}

void
ISizeType::accept_vis (TyVisitor &vis)
{
  vis.visit (*this);
}

std::string
ISizeType::as_string () const
{
  return "isize";
}

BaseType *
ISizeType::unify (BaseType *other)
{
  ISizeRules r (this);
  return r.unify (other);
}

bool
ISizeType::can_eq (BaseType *other, bool emit_errors)
{
  ISizeCmp r (this, emit_errors);
  return r.can_eq (other);
}

BaseType *
ISizeType::clone ()
{
  return new ISizeType (get_ref (), get_ty_ref (), get_combined_refs ());
}

void
CharType::accept_vis (TyVisitor &vis)
{
  vis.visit (*this);
}

std::string
CharType::as_string () const
{
  return "char";
}

BaseType *
CharType::unify (BaseType *other)
{
  CharRules r (this);
  return r.unify (other);
}

bool
CharType::can_eq (BaseType *other, bool emit_errors)
{
  CharCmp r (this, emit_errors);
  return r.can_eq (other);
}

BaseType *
CharType::clone ()
{
  return new CharType (get_ref (), get_ty_ref (), get_combined_refs ());
}

void
ReferenceType::accept_vis (TyVisitor &vis)
{
  vis.visit (*this);
}

std::string
ReferenceType::as_string () const
{
  return "&" + get_base ()->as_string ();
}

BaseType *
ReferenceType::unify (BaseType *other)
{
  ReferenceRules r (this);
  return r.unify (other);
}

bool
ReferenceType::can_eq (BaseType *other, bool emit_errors)
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
  return get_base ()->is_equal (*other2.get_base ());
}

BaseType *
ReferenceType::get_base () const
{
  return base.get_tyty ();
}

BaseType *
ReferenceType::clone ()
{
  return new ReferenceType (get_ref (), get_ty_ref (), base,
			    get_combined_refs ());
}

ReferenceType *
ReferenceType::handle_substitions (SubstitutionArgumentMappings mappings)
{
  auto mappings_table = Analysis::Mappings::get ();

  ReferenceType *ref = static_cast<ReferenceType *> (clone ());
  ref->set_ty_ref (mappings_table->get_next_hir_id ());

  // might be &T or &ADT so this needs to be recursive
  auto base = ref->get_base ();
  BaseType *concrete = Resolver::SubstMapperInternal::Resolve (base, mappings);
  ref->base = TyVar (concrete->get_ty_ref ());

  return ref;
}

void
ParamType::accept_vis (TyVisitor &vis)
{
  vis.visit (*this);
}

std::string
ParamType::as_string () const
{
  if (get_ref () == get_ty_ref ())
    {
      return get_symbol () + " REF: " + std::to_string (get_ref ());
    }

  auto context = Resolver::TypeCheckContext::get ();
  BaseType *lookup = nullptr;
  bool ok = context->lookup_type (get_ty_ref (), &lookup);
  rust_assert (ok);

  return lookup->as_string ();
}

BaseType *
ParamType::unify (BaseType *other)
{
  ParamRules r (this);
  return r.unify (other);
}

bool
ParamType::can_eq (BaseType *other, bool emit_errors)
{
  ParamCmp r (this, emit_errors);
  return r.can_eq (other);
}

BaseType *
ParamType::clone ()
{
  return new ParamType (get_symbol (), get_ref (), get_ty_ref (),
			get_generic_param (), get_combined_refs ());
}

std::string
ParamType::get_symbol () const
{
  return symbol;
}

BaseType *
ParamType::resolve () const
{
  rust_assert (can_resolve ());

  TyVar var (get_ty_ref ());
  BaseType *r = var.get_tyty ();

  while (r->get_kind () == TypeKind::PARAM)
    {
      ParamType *rr = static_cast<ParamType *> (r);
      if (!rr->can_resolve ())
	break;

      TyVar v (rr->get_ty_ref ());
      r = v.get_tyty ();
    }

  return TyVar (r->get_ty_ref ()).get_tyty ();
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
ParamType::handle_substitions (SubstitutionArgumentMappings mappings)
{
  ParamType *p = static_cast<ParamType *> (clone ());

  SubstitutionArg arg = SubstitutionArg::error ();
  bool ok = mappings.get_argument_for_symbol (this, &arg);
  if (ok)
    p->set_ty_ref (arg.get_tyty ()->get_ref ());

  return p;
}

BaseType *
StrType::clone ()
{
  return new StrType (get_ref (), get_ty_ref (), get_combined_refs ());
}

void
StrType::accept_vis (TyVisitor &vis)
{
  vis.visit (*this);
}

std::string
StrType::as_string () const
{
  return "str";
}

BaseType *
StrType::unify (BaseType *other)
{
  StrRules r (this);
  return r.unify (other);
}

bool
StrType::can_eq (BaseType *other, bool emit_errors)
{
  StrCmp r (this, emit_errors);
  return r.can_eq (other);
}

bool
StrType::is_equal (const BaseType &other) const
{
  return get_kind () == other.get_kind ();
}

void
NeverType::accept_vis (TyVisitor &vis)
{
  vis.visit (*this);
}

std::string
NeverType::as_string () const
{
  return "!";
}

BaseType *
NeverType::unify (BaseType *other)
{
  NeverRules r (this);
  return r.unify (other);
}

bool
NeverType::can_eq (BaseType *other, bool emit_errors)
{
  NeverCmp r (this, emit_errors);
  return r.can_eq (other);
}

BaseType *
NeverType::clone ()
{
  return new NeverType (get_ref (), get_ty_ref (), get_combined_refs ());
}

void
PlaceholderType::accept_vis (TyVisitor &vis)
{
  vis.visit (*this);
}

std::string
PlaceholderType::as_string () const
{
  return "<placeholder>";
}

BaseType *
PlaceholderType::unify (BaseType *other)
{
  PlaceholderRules r (this);
  return r.unify (other);
}

bool
PlaceholderType::can_eq (BaseType *other, bool emit_errors)
{
  PlaceholderCmp r (this, emit_errors);
  return r.can_eq (other);
}

BaseType *
PlaceholderType::clone ()
{
  return new PlaceholderType (get_ref (), get_ty_ref (), get_combined_refs ());
}

// rust-tyty-call.h

void
TypeCheckCallExpr::visit (ADTType &type)
{
  if (!type.get_is_tuple ())
    {
      rust_error_at (
	call.get_locus (),
	"expected function, tuple struct or tuple variant, found struct %<%s%>",
	type.get_name ().c_str ());
      return;
    }

  if (call.num_params () != type.num_fields ())
    {
      rust_error_at (call.get_locus (),
		     "unexpected number of arguments %lu expected %lu",
		     call.num_params (), type.num_fields ());
      return;
    }

  size_t i = 0;
  call.iterate_params ([&] (HIR::Expr *p) mutable -> bool {
    StructFieldType *field = type.get_field (i);
    BaseType *field_tyty = field->get_field_type ();

    BaseType *arg = Resolver::TypeCheckExpr::Resolve (p, false);
    if (arg == nullptr)
      {
	rust_error_at (p->get_locus_slow (), "failed to resolve argument type");
	return false;
      }

    auto res = field_tyty->unify (arg);
    if (res == nullptr)
      {
	return false;
      }

    delete res;
    i++;
    return true;
  });

  if (i != call.num_params ())
    {
      rust_error_at (call.get_locus (),
		     "unexpected number of arguments %lu expected %lu", i,
		     call.num_params ());
      return;
    }

  resolved = type.clone ();
}

void
TypeCheckCallExpr::visit (FnType &type)
{
  if (call.num_params () != type.num_params ())
    {
      rust_error_at (call.get_locus (),
		     "unexpected number of arguments %lu expected %lu",
		     call.num_params (), type.num_params ());
      return;
    }

  size_t i = 0;
  call.iterate_params ([&] (HIR::Expr *param) mutable -> bool {
    auto fnparam = type.param_at (i);
    auto argument_expr_tyty = Resolver::TypeCheckExpr::Resolve (param, false);
    if (argument_expr_tyty == nullptr)
      {
	rust_error_at (param->get_locus_slow (),
		       "failed to resolve type for argument expr in CallExpr");
	return false;
      }

    auto resolved_argument_type = fnparam.second->unify (argument_expr_tyty);
    if (resolved_argument_type == nullptr)
      {
	rust_error_at (param->get_locus_slow (),
		       "Type Resolution failure on parameter");
	return false;
      }

    context->insert_type (param->get_mappings (), resolved_argument_type);

    i++;
    return true;
  });

  if (i != call.num_params ())
    {
      rust_error_at (call.get_locus (),
		     "unexpected number of arguments %lu expected %lu", i,
		     call.num_params ());
      return;
    }

  resolved = type.get_return_type ()->clone ();
}

void
TypeCheckCallExpr::visit (FnPtr &type)
{
  if (call.num_params () != type.num_params ())
    {
      rust_error_at (call.get_locus (),
		     "unexpected number of arguments %lu expected %lu",
		     call.num_params (), type.num_params ());
      return;
    }

  size_t i = 0;
  call.iterate_params ([&] (HIR::Expr *param) mutable -> bool {
    auto fnparam = type.param_at (i);
    auto argument_expr_tyty = Resolver::TypeCheckExpr::Resolve (param, false);
    if (argument_expr_tyty == nullptr)
      {
	rust_error_at (param->get_locus_slow (),
		       "failed to resolve type for argument expr in CallExpr");
	return false;
      }

    auto resolved_argument_type = fnparam->unify (argument_expr_tyty);
    if (resolved_argument_type == nullptr)
      {
	rust_error_at (param->get_locus_slow (),
		       "Type Resolution failure on parameter");
	return false;
      }

    context->insert_type (param->get_mappings (), resolved_argument_type);

    i++;
    return true;
  });

  if (i != call.num_params ())
    {
      rust_error_at (call.get_locus (),
		     "unexpected number of arguments %lu expected %lu", i,
		     call.num_params ());
      return;
    }

  resolved = type.get_return_type ()->clone ();
}

// method call checker

void
TypeCheckMethodCallExpr::visit (FnType &type)
{
  // +1 for the receiver self
  size_t num_args_to_call = call.num_params () + 1;
  if (num_args_to_call != type.num_params ())
    {
      rust_error_at (call.get_locus (),
		     "unexpected number of arguments %lu expected %lu",
		     call.num_params (), type.num_params ());
      return;
    }

  size_t i = 1;
  call.iterate_params ([&] (HIR::Expr *param) mutable -> bool {
    auto fnparam = type.param_at (i);
    auto argument_expr_tyty = Resolver::TypeCheckExpr::Resolve (param, false);
    if (argument_expr_tyty == nullptr)
      {
	rust_error_at (param->get_locus_slow (),
		       "failed to resolve type for argument expr in CallExpr");
	return false;
      }

    auto resolved_argument_type = fnparam.second->unify (argument_expr_tyty);
    if (resolved_argument_type == nullptr)
      {
	rust_error_at (param->get_locus_slow (),
		       "Type Resolution failure on parameter");
	return false;
      }

    context->insert_type (param->get_mappings (), resolved_argument_type);

    i++;
    return true;
  });

  if (i != num_args_to_call)
    {
      rust_error_at (call.get_locus (),
		     "unexpected number of arguments %lu expected %lu", i,
		     call.num_params ());
      return;
    }

  resolved = type.get_return_type ()->clone ();
}

} // namespace TyTy
} // namespace Rust
