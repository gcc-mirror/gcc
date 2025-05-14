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

#ifndef RUST_TYTY_CMP_H
#define RUST_TYTY_CMP_H

#include "rust-diagnostics.h"
#include "rust-tyty.h"
#include "rust-tyty-visitor.h"
#include "rust-hir-map.h"
#include "rust-hir-type-check.h"

namespace Rust {
namespace TyTy {

class BaseCmp : public TyConstVisitor
{
public:
  virtual bool can_eq (const BaseType *other)
  {
    if (other->get_kind () == TypeKind::PARAM)
      {
	const ParamType *p = static_cast<const ParamType *> (other);
	other = p->resolve ();
      }
    if (other->get_kind () == TypeKind::PLACEHOLDER)
      {
	const PlaceholderType *p = static_cast<const PlaceholderType *> (other);
	if (p->can_resolve ())
	  {
	    other = p->resolve ();
	  }
      }
    if (other->get_kind () == TypeKind::PROJECTION)
      {
	const ProjectionType *p = static_cast<const ProjectionType *> (other);
	other = p->get ();
      }

    other->accept_vis (*this);
    return ok;
  }

  virtual void visit (const TupleType &type) override
  {
    ok = false;

    if (emit_error_flag)
      {
	location_t ref_locus = mappings.lookup_location (type.get_ref ());
	location_t base_locus
	  = mappings.lookup_location (get_base ()->get_ref ());
	rich_location r (line_table, ref_locus);
	r.add_range (base_locus);
	rust_error_at (r, "expected [%s] got [%s]",
		       get_base ()->as_string ().c_str (),
		       type.as_string ().c_str ());
      }
  }

  virtual void visit (const ADTType &type) override
  {
    ok = false;
    if (emit_error_flag)
      {
	location_t ref_locus = mappings.lookup_location (type.get_ref ());
	location_t base_locus
	  = mappings.lookup_location (get_base ()->get_ref ());
	rich_location r (line_table, ref_locus);
	r.add_range (base_locus);
	rust_error_at (r, "expected [%s] got [%s]",
		       get_base ()->as_string ().c_str (),
		       type.as_string ().c_str ());
      }
  }

  virtual void visit (const InferType &type) override
  {
    ok = false;
    if (emit_error_flag)
      {
	location_t ref_locus = mappings.lookup_location (type.get_ref ());
	location_t base_locus
	  = mappings.lookup_location (get_base ()->get_ref ());
	rich_location r (line_table, ref_locus);
	r.add_range (base_locus);
	rust_error_at (r, "expected [%s] got [%s]",
		       get_base ()->as_string ().c_str (),
		       type.as_string ().c_str ());
      }
  }

  virtual void visit (const FnType &type) override
  {
    ok = false;
    if (emit_error_flag)
      {
	location_t ref_locus = mappings.lookup_location (type.get_ref ());
	location_t base_locus
	  = mappings.lookup_location (get_base ()->get_ref ());
	rich_location r (line_table, ref_locus);
	r.add_range (base_locus);
	rust_error_at (r, "expected [%s] got [%s]",
		       get_base ()->as_string ().c_str (),
		       type.as_string ().c_str ());
      }
  }

  virtual void visit (const FnPtr &type) override
  {
    ok = false;
    if (emit_error_flag)
      {
	location_t ref_locus = mappings.lookup_location (type.get_ref ());
	location_t base_locus
	  = mappings.lookup_location (get_base ()->get_ref ());
	rich_location r (line_table, ref_locus);
	r.add_range (base_locus);
	rust_error_at (r, "expected [%s] got [%s]",
		       get_base ()->as_string ().c_str (),
		       type.as_string ().c_str ());
      }
  }

  virtual void visit (const ArrayType &type) override
  {
    ok = false;
    if (emit_error_flag)
      {
	location_t ref_locus = mappings.lookup_location (type.get_ref ());
	location_t base_locus
	  = mappings.lookup_location (get_base ()->get_ref ());
	rich_location r (line_table, ref_locus);
	r.add_range (base_locus);
	rust_error_at (r, "expected [%s] got [%s]",
		       get_base ()->as_string ().c_str (),
		       type.as_string ().c_str ());
      }
  }

  virtual void visit (const SliceType &type) override
  {
    ok = false;
    if (emit_error_flag)
      {
	location_t ref_locus = mappings.lookup_location (type.get_ref ());
	location_t base_locus
	  = mappings.lookup_location (get_base ()->get_ref ());
	rich_location r (line_table, ref_locus);
	r.add_range (base_locus);
	rust_error_at (r, "expected [%s] got [%s]",
		       get_base ()->as_string ().c_str (),
		       type.as_string ().c_str ());
      }
  }

  virtual void visit (const BoolType &type) override
  {
    ok = false;
    if (emit_error_flag)
      {
	location_t ref_locus = mappings.lookup_location (type.get_ref ());
	location_t base_locus
	  = mappings.lookup_location (get_base ()->get_ref ());
	rich_location r (line_table, ref_locus);
	r.add_range (base_locus);
	rust_error_at (r, "expected [%s] got [%s]",
		       get_base ()->as_string ().c_str (),
		       type.as_string ().c_str ());
      }
  }

  virtual void visit (const IntType &type) override
  {
    ok = false;
    if (emit_error_flag)
      {
	location_t ref_locus = mappings.lookup_location (type.get_ref ());
	location_t base_locus
	  = mappings.lookup_location (get_base ()->get_ref ());
	rich_location r (line_table, ref_locus);
	r.add_range (base_locus);
	rust_error_at (r, "expected [%s] got [%s]",
		       get_base ()->as_string ().c_str (),
		       type.as_string ().c_str ());
      }
  }

  virtual void visit (const UintType &type) override
  {
    ok = false;
    if (emit_error_flag)
      {
	location_t ref_locus = mappings.lookup_location (type.get_ref ());
	location_t base_locus
	  = mappings.lookup_location (get_base ()->get_ref ());
	rich_location r (line_table, ref_locus);
	r.add_range (base_locus);
	rust_error_at (r, "expected [%s] got [%s]",
		       get_base ()->as_string ().c_str (),
		       type.as_string ().c_str ());
      }
  }

  virtual void visit (const USizeType &type) override
  {
    ok = false;
    if (emit_error_flag)
      {
	location_t ref_locus = mappings.lookup_location (type.get_ref ());
	location_t base_locus
	  = mappings.lookup_location (get_base ()->get_ref ());
	rich_location r (line_table, ref_locus);
	r.add_range (base_locus);
	rust_error_at (r, "expected [%s] got [%s]",
		       get_base ()->as_string ().c_str (),
		       type.as_string ().c_str ());
      }
  }

  virtual void visit (const ISizeType &type) override
  {
    ok = false;
    if (emit_error_flag)
      {
	location_t ref_locus = mappings.lookup_location (type.get_ref ());
	location_t base_locus
	  = mappings.lookup_location (get_base ()->get_ref ());
	rich_location r (line_table, ref_locus);
	r.add_range (base_locus);
	rust_error_at (r, "expected [%s] got [%s]",
		       get_base ()->as_string ().c_str (),
		       type.as_string ().c_str ());
      }
  }

  virtual void visit (const FloatType &type) override
  {
    ok = false;
    if (emit_error_flag)
      {
	location_t ref_locus = mappings.lookup_location (type.get_ref ());
	location_t base_locus
	  = mappings.lookup_location (get_base ()->get_ref ());
	rich_location r (line_table, ref_locus);
	r.add_range (base_locus);
	rust_error_at (r, "expected [%s] got [%s]",
		       get_base ()->as_string ().c_str (),
		       type.as_string ().c_str ());
      }
  }

  virtual void visit (const ErrorType &type) override
  {
    ok = false;
    if (emit_error_flag)
      {
	location_t ref_locus = mappings.lookup_location (type.get_ref ());
	location_t base_locus
	  = mappings.lookup_location (get_base ()->get_ref ());
	rich_location r (line_table, ref_locus);
	r.add_range (base_locus);
	rust_error_at (r, "expected [%s] got [%s]",
		       get_base ()->as_string ().c_str (),
		       type.as_string ().c_str ());
      }
  }

  virtual void visit (const CharType &type) override
  {
    ok = false;
    if (emit_error_flag)
      {
	location_t ref_locus = mappings.lookup_location (type.get_ref ());
	location_t base_locus
	  = mappings.lookup_location (get_base ()->get_ref ());
	rich_location r (line_table, ref_locus);
	r.add_range (base_locus);
	rust_error_at (r, "expected [%s] got [%s]",
		       get_base ()->as_string ().c_str (),
		       type.as_string ().c_str ());
      }
  }

  virtual void visit (const ReferenceType &type) override
  {
    ok = false;
    if (emit_error_flag)
      {
	location_t ref_locus = mappings.lookup_location (type.get_ref ());
	location_t base_locus
	  = mappings.lookup_location (get_base ()->get_ref ());
	rich_location r (line_table, ref_locus);
	r.add_range (base_locus);
	rust_error_at (r, "expected [%s] got [%s]",
		       get_base ()->as_string ().c_str (),
		       type.as_string ().c_str ());
      }
  }

  virtual void visit (const PointerType &type) override
  {
    ok = false;
    if (emit_error_flag)
      {
	location_t ref_locus = mappings.lookup_location (type.get_ref ());
	location_t base_locus
	  = mappings.lookup_location (get_base ()->get_ref ());
	rich_location r (line_table, ref_locus);
	r.add_range (base_locus);
	rust_error_at (r, "expected [%s] got [%s]",
		       get_base ()->as_string ().c_str (),
		       type.as_string ().c_str ());
      }
  }

  virtual void visit (const StrType &type) override
  {
    ok = false;
    if (emit_error_flag)
      {
	location_t ref_locus = mappings.lookup_location (type.get_ref ());
	location_t base_locus
	  = mappings.lookup_location (get_base ()->get_ref ());
	rich_location r (line_table, ref_locus);
	r.add_range (base_locus);
	rust_error_at (r, "expected [%s] got [%s]",
		       get_base ()->as_string ().c_str (),
		       type.as_string ().c_str ());
      }
  }

  virtual void visit (const NeverType &type) override
  {
    ok = false;
    if (emit_error_flag)
      {
	location_t ref_locus = mappings.lookup_location (type.get_ref ());
	location_t base_locus
	  = mappings.lookup_location (get_base ()->get_ref ());
	rich_location r (line_table, ref_locus);
	r.add_range (base_locus);
	rust_error_at (r, "expected [%s] got [%s]",
		       get_base ()->as_string ().c_str (),
		       type.as_string ().c_str ());
      }
  }

  virtual void visit (const ProjectionType &type) override
  {
    ok = false;
    if (emit_error_flag)
      {
	location_t ref_locus = mappings.lookup_location (type.get_ref ());
	location_t base_locus
	  = mappings.lookup_location (get_base ()->get_ref ());
	rich_location r (line_table, ref_locus);
	r.add_range (base_locus);
	rust_error_at (r, "expected [%s] got [%s]",
		       get_base ()->as_string ().c_str (),
		       type.as_string ().c_str ());
      }
  }

  virtual void visit (const PlaceholderType &type) override
  {
    // it is ok for types to can eq to a placeholder
    ok = true;
  }

  virtual void visit (const ParamType &type) override
  {
    ok = false;
    if (emit_error_flag)
      {
	location_t ref_locus = mappings.lookup_location (type.get_ref ());
	location_t base_locus
	  = mappings.lookup_location (get_base ()->get_ref ());
	rich_location r (line_table, ref_locus);
	r.add_range (base_locus);
	rust_error_at (r, "expected [%s] got [%s]",
		       get_base ()->as_string ().c_str (),
		       type.as_string ().c_str ());
      }
  }

  virtual void visit (const DynamicObjectType &type) override
  {
    ok = false;
    if (emit_error_flag)
      {
	location_t ref_locus = mappings.lookup_location (type.get_ref ());
	location_t base_locus
	  = mappings.lookup_location (get_base ()->get_ref ());
	rich_location r (line_table, ref_locus);
	r.add_range (base_locus);
	rust_error_at (r, "expected [%s] got [%s]",
		       get_base ()->as_string ().c_str (),
		       type.as_string ().c_str ());
      }
  }

  virtual void visit (const ClosureType &type) override
  {
    ok = false;
    if (emit_error_flag)
      {
	location_t ref_locus = mappings.lookup_location (type.get_ref ());
	location_t base_locus
	  = mappings.lookup_location (get_base ()->get_ref ());
	rich_location r (line_table, ref_locus);
	r.add_range (base_locus);
	rust_error_at (r, "expected [%s] got [%s]",
		       get_base ()->as_string ().c_str (),
		       type.as_string ().c_str ());
      }
  }

  virtual void visit (const OpaqueType &type) override
  {
    ok = false;
    if (emit_error_flag)
      {
	location_t ref_locus = mappings.lookup_location (type.get_ref ());
	location_t base_locus
	  = mappings.lookup_location (get_base ()->get_ref ());
	rich_location r (line_table, ref_locus);
	r.add_range (base_locus);
	rust_error_at (r, "expected [%s] got [%s]",
		       get_base ()->as_string ().c_str (),
		       type.as_string ().c_str ());
      }
  }

protected:
  BaseCmp (const BaseType *base, bool emit_errors)
    : mappings (Analysis::Mappings::get ()),
      context (Resolver::TypeCheckContext::get ()), ok (false),
      emit_error_flag (emit_errors)
  {}

  Analysis::Mappings &mappings;
  Resolver::TypeCheckContext *context;

  bool ok;
  bool emit_error_flag;

private:
  /* Returns a pointer to the ty that created this rule. */
  virtual const BaseType *get_base () const = 0;
};

class InferCmp : public BaseCmp
{
  using Rust::TyTy::BaseCmp::visit;

public:
  InferCmp (const InferType *base, bool emit_errors)
    : BaseCmp (base, emit_errors), base (base)
  {}

  void visit (const BoolType &type) override
  {
    bool is_valid
      = (base->get_infer_kind () == TyTy::InferType::InferTypeKind::GENERAL);
    if (is_valid)
      {
	ok = true;
	return;
      }

    BaseCmp::visit (type);
  }

  void visit (const IntType &type) override
  {
    bool is_valid
      = (base->get_infer_kind () == TyTy::InferType::InferTypeKind::GENERAL)
	|| (base->get_infer_kind ()
	    == TyTy::InferType::InferTypeKind::INTEGRAL);
    if (is_valid)
      {
	ok = true;
	return;
      }

    BaseCmp::visit (type);
  }

  void visit (const UintType &type) override
  {
    bool is_valid
      = (base->get_infer_kind () == TyTy::InferType::InferTypeKind::GENERAL)
	|| (base->get_infer_kind ()
	    == TyTy::InferType::InferTypeKind::INTEGRAL);
    if (is_valid)
      {
	ok = true;
	return;
      }

    BaseCmp::visit (type);
  }

  void visit (const USizeType &type) override
  {
    bool is_valid
      = (base->get_infer_kind () == TyTy::InferType::InferTypeKind::GENERAL)
	|| (base->get_infer_kind ()
	    == TyTy::InferType::InferTypeKind::INTEGRAL);
    if (is_valid)
      {
	ok = true;
	return;
      }

    BaseCmp::visit (type);
  }

  void visit (const ISizeType &type) override
  {
    bool is_valid
      = (base->get_infer_kind () == TyTy::InferType::InferTypeKind::GENERAL)
	|| (base->get_infer_kind ()
	    == TyTy::InferType::InferTypeKind::INTEGRAL);
    if (is_valid)
      {
	ok = true;
	return;
      }

    BaseCmp::visit (type);
  }

  void visit (const FloatType &type) override
  {
    bool is_valid
      = (base->get_infer_kind () == TyTy::InferType::InferTypeKind::GENERAL)
	|| (base->get_infer_kind () == TyTy::InferType::InferTypeKind::FLOAT);
    if (is_valid)
      {
	ok = true;
	return;
      }

    BaseCmp::visit (type);
  }

  void visit (const ArrayType &type) override
  {
    bool is_valid
      = (base->get_infer_kind () == TyTy::InferType::InferTypeKind::GENERAL);
    if (is_valid)
      {
	ok = true;
	return;
      }

    BaseCmp::visit (type);
  }

  void visit (const SliceType &type) override
  {
    bool is_valid
      = (base->get_infer_kind () == TyTy::InferType::InferTypeKind::GENERAL);
    if (is_valid)
      {
	ok = true;
	return;
      }

    BaseCmp::visit (type);
  }

  void visit (const ADTType &type) override
  {
    bool is_valid
      = (base->get_infer_kind () == TyTy::InferType::InferTypeKind::GENERAL);
    if (is_valid)
      {
	ok = true;
	return;
      }

    BaseCmp::visit (type);
  }

  void visit (const TupleType &type) override
  {
    bool is_valid
      = (base->get_infer_kind () == TyTy::InferType::InferTypeKind::GENERAL);
    if (is_valid)
      {
	ok = true;
	return;
      }

    BaseCmp::visit (type);
  }

  void visit (const InferType &type) override
  {
    switch (base->get_infer_kind ())
      {
      case InferType::InferTypeKind::GENERAL:
	ok = true;
	return;

	case InferType::InferTypeKind::INTEGRAL: {
	  if (type.get_infer_kind () == InferType::InferTypeKind::INTEGRAL)
	    {
	      ok = true;
	      return;
	    }
	  else if (type.get_infer_kind () == InferType::InferTypeKind::GENERAL)
	    {
	      ok = true;
	      return;
	    }
	}
	break;

	case InferType::InferTypeKind::FLOAT: {
	  if (type.get_infer_kind () == InferType::InferTypeKind::FLOAT)
	    {
	      ok = true;
	      return;
	    }
	  else if (type.get_infer_kind () == InferType::InferTypeKind::GENERAL)
	    {
	      ok = true;
	      return;
	    }
	}
	break;
      }

    BaseCmp::visit (type);
  }

  void visit (const CharType &type) override
  {
    {
      bool is_valid
	= (base->get_infer_kind () == TyTy::InferType::InferTypeKind::GENERAL);
      if (is_valid)
	{
	  ok = true;
	  return;
	}

      BaseCmp::visit (type);
    }
  }

  void visit (const ReferenceType &type) override
  {
    bool is_valid
      = (base->get_infer_kind () == TyTy::InferType::InferTypeKind::GENERAL);
    if (is_valid)
      {
	ok = true;
	return;
      }

    BaseCmp::visit (type);
  }

  void visit (const PointerType &type) override
  {
    bool is_valid
      = (base->get_infer_kind () == TyTy::InferType::InferTypeKind::GENERAL);
    if (is_valid)
      {
	ok = true;
	return;
      }

    BaseCmp::visit (type);
  }

  void visit (const ParamType &) override { ok = true; }

  void visit (const DynamicObjectType &type) override
  {
    bool is_valid
      = (base->get_infer_kind () == TyTy::InferType::InferTypeKind::GENERAL);
    if (is_valid)
      {
	ok = true;
	return;
      }

    BaseCmp::visit (type);
  }

  void visit (const ClosureType &type) override
  {
    bool is_valid
      = (base->get_infer_kind () == TyTy::InferType::InferTypeKind::GENERAL);
    if (is_valid)
      {
	ok = true;
	return;
      }

    BaseCmp::visit (type);
  }

private:
  const BaseType *get_base () const override { return base; }
  const InferType *base;
};

class FnCmp : public BaseCmp
{
  using Rust::TyTy::BaseCmp::visit;

public:
  FnCmp (const FnType *base, bool emit_errors)
    : BaseCmp (base, emit_errors), base (base)
  {}

  void visit (const InferType &type) override
  {
    ok = type.get_infer_kind () == InferType::InferTypeKind::GENERAL;
  }

  void visit (const FnType &type) override
  {
    if (base->num_params () != type.num_params ())
      {
	BaseCmp::visit (type);
	return;
      }

    for (size_t i = 0; i < base->num_params (); i++)
      {
	auto a = base->param_at (i).get_type ();
	auto b = type.param_at (i).get_type ();

	if (!a->can_eq (b, emit_error_flag))
	  {
	    emit_error_flag = false;
	    BaseCmp::visit (type);
	    return;
	  }
      }

    if (!base->get_return_type ()->can_eq (type.get_return_type (),
					   emit_error_flag))
      {
	emit_error_flag = false;
	BaseCmp::visit (type);
	return;
      }

    ok = true;
  }

private:
  const BaseType *get_base () const override { return base; }
  const FnType *base;
};

class FnptrCmp : public BaseCmp
{
  using Rust::TyTy::BaseCmp::visit;

public:
  FnptrCmp (const FnPtr *base, bool emit_errors)
    : BaseCmp (base, emit_errors), base (base)
  {}

  void visit (const InferType &type) override
  {
    if (type.get_infer_kind () != InferType::InferTypeKind::GENERAL)
      {
	BaseCmp::visit (type);
	return;
      }

    ok = true;
  }

  void visit (const FnPtr &type) override
  {
    if (base->num_params () != type.num_params ())
      {
	BaseCmp::visit (type);
	return;
      }

    auto this_ret_type = base->get_return_type ();
    auto other_ret_type = type.get_return_type ();
    if (!this_ret_type->can_eq (other_ret_type, emit_error_flag))
      {
	BaseCmp::visit (type);
	return;
      }

    for (size_t i = 0; i < base->num_params (); i++)
      {
	auto this_param = base->get_param_type_at (i);
	auto other_param = type.get_param_type_at (i);
	if (!this_param->can_eq (other_param, emit_error_flag))
	  {
	    BaseCmp::visit (type);
	    return;
	  }
      }

    ok = true;
  }

  void visit (const FnType &type) override
  {
    if (base->num_params () != type.num_params ())
      {
	BaseCmp::visit (type);
	return;
      }

    auto this_ret_type = base->get_return_type ();
    auto other_ret_type = type.get_return_type ();
    if (!this_ret_type->can_eq (other_ret_type, emit_error_flag))
      {
	BaseCmp::visit (type);
	return;
      }

    for (size_t i = 0; i < base->num_params (); i++)
      {
	auto this_param = base->get_param_type_at (i);
	auto other_param = type.param_at (i).get_type ();
	if (!this_param->can_eq (other_param, emit_error_flag))
	  {
	    BaseCmp::visit (type);
	    return;
	  }
      }

    ok = true;
  }

private:
  const BaseType *get_base () const override { return base; }
  const FnPtr *base;
};

class ClosureCmp : public BaseCmp
{
  using Rust::TyTy::BaseCmp::visit;

public:
  ClosureCmp (const ClosureType *base, bool emit_errors)
    : BaseCmp (base, emit_errors), base (base)
  {}

  void visit (const InferType &type) override
  {
    if (type.get_infer_kind () != InferType::InferTypeKind::GENERAL)
      {
	BaseCmp::visit (type);
	return;
      }

    ok = true;
  }

  void visit (const ClosureType &type) override
  {
    if (base->get_def_id () != type.get_def_id ())
      {
	BaseCmp::visit (type);
	return;
      }

    if (!base->get_parameters ().can_eq (&type.get_parameters (), false))
      {
	BaseCmp::visit (type);
	return;
      }

    if (!base->get_result_type ().can_eq (&type.get_result_type (), false))
      {
	BaseCmp::visit (type);
	return;
      }

    ok = true;
  }

private:
  const BaseType *get_base () const override { return base; }
  const ClosureType *base;
};

class ArrayCmp : public BaseCmp
{
  using Rust::TyTy::BaseCmp::visit;

public:
  ArrayCmp (const ArrayType *base, bool emit_errors)
    : BaseCmp (base, emit_errors), base (base)
  {}

  void visit (const ArrayType &type) override
  {
    // check base type
    const BaseType *base_element = base->get_element_type ();
    const BaseType *other_element = type.get_element_type ();
    if (!base_element->can_eq (other_element, emit_error_flag))
      {
	BaseCmp::visit (type);
	return;
      }

    ok = true;
  }

  void visit (const InferType &type) override
  {
    if (type.get_infer_kind () != InferType::InferTypeKind::GENERAL)
      {
	BaseCmp::visit (type);
	return;
      }

    ok = true;
  }

private:
  const BaseType *get_base () const override { return base; }
  const ArrayType *base;
};

class SliceCmp : public BaseCmp
{
  using Rust::TyTy::BaseCmp::visit;

public:
  SliceCmp (const SliceType *base, bool emit_errors)
    : BaseCmp (base, emit_errors), base (base)
  {}

  void visit (const SliceType &type) override
  {
    // check base type
    const BaseType *base_element = base->get_element_type ();
    const BaseType *other_element = type.get_element_type ();
    if (!base_element->can_eq (other_element, emit_error_flag))
      {
	BaseCmp::visit (type);
	return;
      }

    ok = true;
  }

  void visit (const InferType &type) override
  {
    if (type.get_infer_kind () != InferType::InferTypeKind::GENERAL)
      {
	BaseCmp::visit (type);
	return;
      }

    ok = true;
  }

private:
  const BaseType *get_base () const override { return base; }
  const SliceType *base;
};

class BoolCmp : public BaseCmp
{
  using Rust::TyTy::BaseCmp::visit;

public:
  BoolCmp (const BoolType *base, bool emit_errors)
    : BaseCmp (base, emit_errors), base (base)
  {}

  void visit (const BoolType &type) override { ok = true; }

  void visit (const InferType &type) override
  {
    ok = type.get_infer_kind () == InferType::InferTypeKind::GENERAL;
  }

private:
  const BaseType *get_base () const override { return base; }
  const BoolType *base;
};

class IntCmp : public BaseCmp
{
  using Rust::TyTy::BaseCmp::visit;

public:
  IntCmp (const IntType *base, bool emit_errors)
    : BaseCmp (base, emit_errors), base (base)
  {}

  void visit (const InferType &type) override
  {
    ok = type.get_infer_kind () != InferType::InferTypeKind::FLOAT;
  }

  void visit (const IntType &type) override
  {
    ok = type.get_int_kind () == base->get_int_kind ();
  }

private:
  const BaseType *get_base () const override { return base; }
  const IntType *base;
};

class UintCmp : public BaseCmp
{
  using Rust::TyTy::BaseCmp::visit;

public:
  UintCmp (const UintType *base, bool emit_errors)
    : BaseCmp (base, emit_errors), base (base)
  {}

  void visit (const InferType &type) override
  {
    ok = type.get_infer_kind () != InferType::InferTypeKind::FLOAT;
  }

  void visit (const UintType &type) override
  {
    ok = type.get_uint_kind () == base->get_uint_kind ();
  }

private:
  const BaseType *get_base () const override { return base; }
  const UintType *base;
};

class FloatCmp : public BaseCmp
{
  using Rust::TyTy::BaseCmp::visit;

public:
  FloatCmp (const FloatType *base, bool emit_errors)
    : BaseCmp (base, emit_errors), base (base)
  {}

  void visit (const InferType &type) override
  {
    ok = type.get_infer_kind () != InferType::InferTypeKind::INTEGRAL;
  }

  void visit (const FloatType &type) override
  {
    ok = type.get_float_kind () == base->get_float_kind ();
  }

private:
  const BaseType *get_base () const override { return base; }
  const FloatType *base;
};

class ADTCmp : public BaseCmp
{
  using Rust::TyTy::BaseCmp::visit;

public:
  ADTCmp (const ADTType *base, bool emit_errors)
    : BaseCmp (base, emit_errors), base (base)
  {}

  void visit (const ADTType &type) override
  {
    if (base->get_adt_kind () != type.get_adt_kind ())
      {
	BaseCmp::visit (type);
	return;
      }

    if (base->get_identifier ().compare (type.get_identifier ()) != 0)
      {
	BaseCmp::visit (type);
	return;
      }

    if (base->number_of_variants () != type.number_of_variants ())
      {
	BaseCmp::visit (type);
	return;
      }

    for (size_t i = 0; i < type.number_of_variants (); ++i)
      {
	TyTy::VariantDef *a = base->get_variants ().at (i);
	TyTy::VariantDef *b = type.get_variants ().at (i);

	if (a->num_fields () != b->num_fields ())
	  {
	    BaseCmp::visit (type);
	    return;
	  }

	for (size_t j = 0; j < a->num_fields (); j++)
	  {
	    TyTy::StructFieldType *base_field = a->get_field_at_index (j);
	    TyTy::StructFieldType *other_field = b->get_field_at_index (j);

	    TyTy::BaseType *this_field_ty = base_field->get_field_type ();
	    TyTy::BaseType *other_field_ty = other_field->get_field_type ();

	    if (!this_field_ty->can_eq (other_field_ty, emit_error_flag))
	      {
		BaseCmp::visit (type);
		return;
	      }
	  }
      }

    ok = true;
  }

  void visit (const InferType &type) override
  {
    if (type.get_infer_kind () != InferType::InferTypeKind::GENERAL)
      {
	BaseCmp::visit (type);
	return;
      }

    ok = true;
  }

private:
  const BaseType *get_base () const override { return base; }
  const ADTType *base;
};

class TupleCmp : public BaseCmp
{
  using Rust::TyTy::BaseCmp::visit;

public:
  TupleCmp (const TupleType *base, bool emit_errors)
    : BaseCmp (base, emit_errors), base (base)
  {}

  void visit (const TupleType &type) override
  {
    if (base->num_fields () != type.num_fields ())
      {
	BaseCmp::visit (type);
	return;
      }

    for (size_t i = 0; i < base->num_fields (); i++)
      {
	BaseType *bo = base->get_field (i);
	BaseType *fo = type.get_field (i);

	if (!bo->can_eq (fo, emit_error_flag))
	  {
	    BaseCmp::visit (type);
	    return;
	  }
      }

    ok = true;
  }

  void visit (const InferType &type) override
  {
    if (type.get_infer_kind () != InferType::InferTypeKind::GENERAL)
      {
	BaseCmp::visit (type);
	return;
      }

    ok = true;
  }

private:
  const BaseType *get_base () const override { return base; }
  const TupleType *base;
};

class USizeCmp : public BaseCmp
{
  using Rust::TyTy::BaseCmp::visit;

public:
  USizeCmp (const USizeType *base, bool emit_errors)
    : BaseCmp (base, emit_errors), base (base)
  {}

  void visit (const InferType &type) override
  {
    ok = type.get_infer_kind () != InferType::InferTypeKind::FLOAT;
  }

  void visit (const USizeType &type) override { ok = true; }

private:
  const BaseType *get_base () const override { return base; }
  const USizeType *base;
};

class ISizeCmp : public BaseCmp
{
  using Rust::TyTy::BaseCmp::visit;

public:
  ISizeCmp (const ISizeType *base, bool emit_errors)
    : BaseCmp (base, emit_errors), base (base)
  {}

  void visit (const InferType &type) override
  {
    ok = type.get_infer_kind () != InferType::InferTypeKind::FLOAT;
  }

  void visit (const ISizeType &type) override { ok = true; }

private:
  const BaseType *get_base () const override { return base; }
  const ISizeType *base;
};

class CharCmp : public BaseCmp
{
  using Rust::TyTy::BaseCmp::visit;

public:
  CharCmp (const CharType *base, bool emit_errors)
    : BaseCmp (base, emit_errors), base (base)
  {}

  void visit (const InferType &type) override
  {
    ok = type.get_infer_kind () == InferType::InferTypeKind::GENERAL;
  }

  void visit (const CharType &type) override { ok = true; }

private:
  const BaseType *get_base () const override { return base; }
  const CharType *base;
};

class ReferenceCmp : public BaseCmp
{
  using Rust::TyTy::BaseCmp::visit;

public:
  ReferenceCmp (const ReferenceType *base, bool emit_errors)
    : BaseCmp (base, emit_errors), base (base)
  {}

  void visit (const ReferenceType &type) override
  {
    auto base_type = base->get_base ();
    auto other_base_type = type.get_base ();

    bool mutability_ok = base->is_mutable () ? type.is_mutable () : true;
    if (!mutability_ok)
      {
	BaseCmp::visit (type);
	return;
      }

    if (!base_type->can_eq (other_base_type, emit_error_flag))
      {
	BaseCmp::visit (type);
	return;
      }

    ok = true;
  }

  void visit (const InferType &type) override
  {
    if (type.get_infer_kind () != InferType::InferTypeKind::GENERAL)
      {
	BaseCmp::visit (type);
	return;
      }

    ok = true;
  }

private:
  const BaseType *get_base () const override { return base; }
  const ReferenceType *base;
};

class PointerCmp : public BaseCmp
{
  using Rust::TyTy::BaseCmp::visit;

public:
  PointerCmp (const PointerType *base, bool emit_errors)
    : BaseCmp (base, emit_errors), base (base)
  {}

  void visit (const PointerType &type) override
  {
    auto base_type = base->get_base ();
    auto other_base_type = type.get_base ();

    bool mutability_ok = base->is_mutable () ? type.is_mutable () : true;
    if (!mutability_ok)
      {
	BaseCmp::visit (type);
	return;
      }

    if (!base_type->can_eq (other_base_type, emit_error_flag))
      {
	BaseCmp::visit (type);
	return;
      }

    ok = true;
  }

  void visit (const InferType &type) override
  {
    if (type.get_infer_kind () != InferType::InferTypeKind::GENERAL)
      {
	BaseCmp::visit (type);
	return;
      }

    ok = true;
  }

private:
  const BaseType *get_base () const override { return base; }
  const PointerType *base;
};

class ParamCmp : public BaseCmp
{
  using Rust::TyTy::BaseCmp::visit;

public:
  ParamCmp (const ParamType *base, bool emit_errors)
    : BaseCmp (base, emit_errors), base (base)
  {}

  // param types are a placeholder we shouldn't have cases where we unify
  // against it. eg: struct foo<T> { a: T }; When we invoke it we can do either:
  //
  // foo<i32>{ a: 123 }.
  // Then this enforces the i32 type to be referenced on the
  // field via an hirid.
  //
  // rust also allows for a = foo{a:123}; Where we can use an Inference Variable
  // to handle the typing of the struct
  bool can_eq (const BaseType *other) override
  {
    if (!base->can_resolve ())
      return BaseCmp::can_eq (other);

    auto lookup = base->resolve ();
    return lookup->can_eq (other, emit_error_flag);
  }

  // imagine the case where we have:
  // struct Foo<T>(T);
  // Then we declare a generic impl block
  // impl <X>Foo<X> { ... }
  // both of these types are compatible so we mostly care about the number of
  // generic arguments
  void visit (const ParamType &) override { ok = true; }

  void visit (const TupleType &) override { ok = true; }

  void visit (const InferType &) override { ok = true; }

  void visit (const FnType &) override { ok = true; }

  void visit (const FnPtr &) override { ok = true; }

  void visit (const ADTType &) override { ok = true; }

  void visit (const ArrayType &) override { ok = true; }

  void visit (const SliceType &) override { ok = true; }

  void visit (const BoolType &) override { ok = true; }

  void visit (const IntType &) override { ok = true; }

  void visit (const UintType &) override { ok = true; }

  void visit (const USizeType &) override { ok = true; }

  void visit (const ISizeType &) override { ok = true; }

  void visit (const FloatType &) override { ok = true; }

  void visit (const CharType &) override { ok = true; }

  void visit (const ReferenceType &) override { ok = true; }

  void visit (const PointerType &) override { ok = true; }

  void visit (const StrType &) override { ok = true; }

  void visit (const NeverType &) override { ok = true; }

  void visit (const DynamicObjectType &) override { ok = true; }

  void visit (const PlaceholderType &type) override
  {
    ok = base->get_symbol ().compare (type.get_symbol ()) == 0;
  }

private:
  const BaseType *get_base () const override { return base; }
  const ParamType *base;
};

class StrCmp : public BaseCmp
{
  // FIXME we will need a enum for the StrType like ByteBuf etc..
  using Rust::TyTy::BaseCmp::visit;

public:
  StrCmp (const StrType *base, bool emit_errors)
    : BaseCmp (base, emit_errors), base (base)
  {}

  void visit (const StrType &type) override { ok = true; }

  void visit (const InferType &type) override
  {
    if (type.get_infer_kind () != InferType::InferTypeKind::GENERAL)
      {
	BaseCmp::visit (type);
	return;
      }

    ok = true;
  }

private:
  const BaseType *get_base () const override { return base; }
  const StrType *base;
};

class NeverCmp : public BaseCmp
{
  using Rust::TyTy::BaseCmp::visit;

public:
  NeverCmp (const NeverType *base, bool emit_errors)
    : BaseCmp (base, emit_errors), base (base)
  {}

  void visit (const NeverType &type) override { ok = true; }

  void visit (const InferType &type) override
  {
    if (type.get_infer_kind () != InferType::InferTypeKind::GENERAL)
      {
	BaseCmp::visit (type);
	return;
      }

    ok = true;
  }

private:
  const BaseType *get_base () const override { return base; }
  const NeverType *base;
};

class PlaceholderCmp : public BaseCmp
{
  using Rust::TyTy::BaseCmp::visit;

public:
  PlaceholderCmp (const PlaceholderType *base, bool emit_errors)
    : BaseCmp (base, emit_errors), base (base)
  {}

  bool can_eq (const BaseType *other) override
  {
    if (!base->can_resolve ())
      return BaseCmp::can_eq (other);

    BaseType *lookup = base->resolve ();
    return lookup->can_eq (other, emit_error_flag);
  }

  void visit (const TupleType &) override { ok = true; }

  void visit (const ADTType &) override { ok = true; }

  void visit (const InferType &) override { ok = true; }

  void visit (const FnType &) override { ok = true; }

  void visit (const FnPtr &) override { ok = true; }

  void visit (const ArrayType &) override { ok = true; }

  void visit (const BoolType &) override { ok = true; }

  void visit (const IntType &) override { ok = true; }

  void visit (const UintType &) override { ok = true; }

  void visit (const USizeType &) override { ok = true; }

  void visit (const ISizeType &) override { ok = true; }

  void visit (const FloatType &) override { ok = true; }

  void visit (const ErrorType &) override { ok = true; }

  void visit (const CharType &) override { ok = true; }

  void visit (const ReferenceType &) override { ok = true; }

  void visit (const ParamType &) override { ok = true; }

  void visit (const StrType &) override { ok = true; }

  void visit (const NeverType &) override { ok = true; }

  void visit (const SliceType &) override { ok = true; }

private:
  const BaseType *get_base () const override { return base; }

  const PlaceholderType *base;
};

class DynamicCmp : public BaseCmp
{
  using Rust::TyTy::BaseCmp::visit;

public:
  DynamicCmp (const DynamicObjectType *base, bool emit_errors)
    : BaseCmp (base, emit_errors), base (base)
  {}

  void visit (const DynamicObjectType &type) override
  {
    if (base->num_specified_bounds () != type.num_specified_bounds ())
      {
	BaseCmp::visit (type);
	return;
      }

    location_t ref_locus = mappings.lookup_location (type.get_ref ());
    ok = base->bounds_compatible (type, ref_locus, false);
  }

private:
  const BaseType *get_base () const override { return base; }

  const DynamicObjectType *base;
};

class OpaqueCmp : public BaseCmp
{
  using Rust::TyTy::BaseCmp::visit;

public:
  OpaqueCmp (const OpaqueType *base, bool emit_errors)
    : BaseCmp (base, emit_errors), base (base)
  {}

  // TODO

private:
  const BaseType *get_base () const override { return base; }

  const OpaqueType *base;
};

} // namespace TyTy
} // namespace Rust

#endif // RUST_TYTY_CMP_H
