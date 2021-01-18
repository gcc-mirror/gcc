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

#ifndef RUST_HIR_TYPE_CHECK_TYPE
#define RUST_HIR_TYPE_CHECK_TYPE

#include "rust-hir-type-check-base.h"
#include "rust-hir-full.h"

namespace Rust {
namespace Resolver {

class ArrayCapacityConstant : public TypeCheckBase
{
public:
  static bool fold (HIR::Expr *expr, size_t *folded_result)
  {
    ArrayCapacityConstant folder;
    expr->accept_vis (folder);
    *folded_result = folder.result;
    return folder.ok;
  }

  virtual ~ArrayCapacityConstant () {}

  void visit (HIR::LiteralExpr &expr)
  {
    switch (expr.get_lit_type ())
      {
	case HIR::Literal::LitType::INT: {
	  ok = true;
	  std::stringstream ss (expr.as_string ());
	  ss >> result;
	}
	break;

      default:
	return;
      }
  }

private:
  ArrayCapacityConstant () : TypeCheckBase (), ok (false), result (-1) {}

  bool ok;
  size_t result;
}; // namespace Resolver

class TypeCheckType : public TypeCheckBase
{
public:
  static TyTy::TyBase *Resolve (HIR::Type *type)
  {
    TypeCheckType resolver;
    type->accept_vis (resolver);

    if (resolver.translated != nullptr)
      resolver.context->insert_type (type->get_mappings ().get_hirid (),
				     resolver.translated);

    return resolver.translated;
  }

  void visit (HIR::TupleType &tuple)
  {
    if (tuple.is_unit_type ())
      {
	auto unit_node_id = resolver->get_unit_type_node_id ();
	if (!context->lookup_builtin (unit_node_id, &translated))
	  {
	    rust_error_at (tuple.get_locus (),
			   "failed to lookup builtin unit type");
	  }
	return;
      }

    size_t index = 0;
    std::string identifier = "(";
    std::vector<TyTy::StructFieldType *> fields;
    for (auto &elem : tuple.get_elems ())
      {
	auto field_ty = TypeCheckType::Resolve (elem.get ());
	identifier += field_ty->as_string ();
	if ((index + 1) < tuple.get_elems ().size ())
	  identifier += ",";

	auto field_tyty
	  = new TyTy::StructFieldType (elem->get_mappings ().get_hirid (),
				       std::to_string (index), field_ty);
	fields.push_back (field_tyty);
	index++;
      }
    identifier += ")";
    translated = new TyTy::ADTType (tuple.get_mappings ().get_hirid (),
				    identifier, true, fields);
  }

  void visit (HIR::TypePath &path)
  {
    // check if this is already defined or not
    if (context->lookup_type (path.get_mappings ().get_hirid (), &translated))
      return;

    // lookup the Node this resolves to
    NodeId ref;
    if (!resolver->lookup_resolved_type (path.get_mappings ().get_nodeid (),
					 &ref))
      {
	rust_fatal_error (path.get_locus (),
			  "Failed to resolve node id to HIR");
	return;
      }

    // reverse lookup the hir node from ast node id
    HirId hir_lookup;
    if (context->lookup_type_by_node_id (ref, &hir_lookup))
      {
	// we got an HIR node
	if (context->lookup_type (hir_lookup, &translated))
	  return;
      }

    // this might be a struct type (TyTy::ADT) reference
    // TODO
    gcc_unreachable ();
  }

  void visit (HIR::ArrayType &type)
  {
    size_t capacity;
    if (!ArrayCapacityConstant::fold (type.get_size_expr (), &capacity))
      {
	rust_error_at (type.get_size_expr ()->get_locus_slow (),
		       "non-constant value");
	return;
      }

    TyTy::TyBase *base = TypeCheckType::Resolve (type.get_element_type ());
    translated
      = new TyTy::ArrayType (type.get_mappings ().get_hirid (), capacity, base);
  }

private:
  TypeCheckType () : TypeCheckBase (), translated (nullptr) {}

  TyTy::TyBase *translated;
};

} // namespace Resolver
} // namespace Rust

#endif // RUST_HIR_TYPE_CHECK_TYPE
