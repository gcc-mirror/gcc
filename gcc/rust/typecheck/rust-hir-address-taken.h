// Copyright (C) 2020-2022 Free Software Foundation, Inc.

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

#ifndef RUST_HIR_ADDRESS_TAKEN
#define RUST_HIR_ADDRESS_TAKEN

#include "rust-hir-type-check-base.h"

namespace Rust {
namespace Resolver {

class AddressTakenContext
{
public:
  static AddressTakenContext *get ();

  ~AddressTakenContext ();

  bool lookup_addess_taken (HirId id, bool *address_taken) const;

  void insert_address_taken (HirId id, bool address_taken);

private:
  std::map<HirId, bool> ctx;
};

class AddressTakenResolver : public TypeCheckBase
{
  using Rust::Resolver::TypeCheckBase::visit;

public:
  static void SetAddressTaken (HIR::Expr &expr)
  {
    AddressTakenResolver resolver;
    expr.accept_vis (resolver);
  }

  void visit (HIR::IdentifierExpr &expr) override
  {
    NodeId ast_node_id = expr.get_mappings ().get_nodeid ();
    NodeId ref_node_id = UNKNOWN_NODEID;
    if (resolver->lookup_resolved_name (ast_node_id, &ref_node_id))
      {
	// these ref_node_ids will resolve to a pattern declaration but we are
	// interested in the definition that this refers to get the parent id
	Definition def;
	if (!resolver->lookup_definition (ref_node_id, &def))
	  {
	    rust_error_at (expr.get_locus (),
			   "unknown reference for resolved name");
	    return;
	  }
	ref_node_id = def.parent;
      }

    if (ref_node_id == UNKNOWN_NODEID)
      return;

    // node back to HIR
    HirId ref = UNKNOWN_HIRID;
    bool reverse_lookup
      = mappings->lookup_node_to_hir (expr.get_mappings ().get_crate_num (),
				      ref_node_id, &ref);
    rust_assert (reverse_lookup);
    context->insert_address_taken (ref, true);
  }

  void visit (HIR::PathInExpression &expr) override
  {
    NodeId ast_node_id = expr.get_mappings ().get_nodeid ();
    NodeId ref_node_id = UNKNOWN_NODEID;
    if (resolver->lookup_resolved_name (ast_node_id, &ref_node_id))
      {
	// these ref_node_ids will resolve to a pattern declaration but we are
	// interested in the definition that this refers to get the parent id
	Definition def;
	if (!resolver->lookup_definition (ref_node_id, &def))
	  {
	    rust_error_at (expr.get_locus (),
			   "unknown reference for resolved name");
	    return;
	  }
	ref_node_id = def.parent;
      }

    if (ref_node_id == UNKNOWN_NODEID)
      return;

    // node back to HIR
    HirId ref = UNKNOWN_HIRID;
    bool reverse_lookup
      = mappings->lookup_node_to_hir (expr.get_mappings ().get_crate_num (),
				      ref_node_id, &ref);
    rust_assert (reverse_lookup);
    context->insert_address_taken (ref, true);
  }

  void visit (HIR::QualifiedPathInExpression &expr) override
  {
    NodeId ast_node_id = expr.get_mappings ().get_nodeid ();
    NodeId ref_node_id = UNKNOWN_NODEID;
    if (resolver->lookup_resolved_name (ast_node_id, &ref_node_id))
      {
	// these ref_node_ids will resolve to a pattern declaration but we are
	// interested in the definition that this refers to get the parent id
	Definition def;
	if (!resolver->lookup_definition (ref_node_id, &def))
	  {
	    rust_error_at (expr.get_locus (),
			   "unknown reference for resolved name");
	    return;
	  }
	ref_node_id = def.parent;
      }

    if (ref_node_id == UNKNOWN_NODEID)
      return;

    // node back to HIR
    HirId ref = UNKNOWN_HIRID;
    bool reverse_lookup
      = mappings->lookup_node_to_hir (expr.get_mappings ().get_crate_num (),
				      ref_node_id, &ref);
    rust_assert (reverse_lookup);
    context->insert_address_taken (ref, true);
  }

  void visit (HIR::DereferenceExpr &expr) override
  {
    expr.get_expr ()->accept_vis (*this);
  }

private:
  AddressTakenResolver ()
    : TypeCheckBase (), context (AddressTakenContext::get ())
  {}

  AddressTakenContext *context;
};

} // namespace Resolver
} // namespace Rust

#endif // RUST_HIR_ADDRESS_TAKEN
