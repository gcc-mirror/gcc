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

#ifndef RUST_AST_RESOLVE_IMPLITEM_H
#define RUST_AST_RESOLVE_IMPLITEM_H

#include "rust-ast-resolve-base.h"
#include "rust-ast-resolve-type.h"
#include "rust-ast-full.h"

namespace Rust {
namespace Resolver {

class ResolveToplevelImplItem : public ResolverBase
{
  using Rust::Resolver::ResolverBase::visit;

public:
  static void go (AST::InherentImplItem *item, AST::Type *base)
  {
    ResolveToplevelImplItem resolver (base);
    if (resolver.base_path.is_empty ())
      {
	rust_error_at (base->get_locus_slow (),
		       "failed to resolve simple path");
	return;
      }
    item->accept_vis (resolver);
  }

  void visit (AST::ConstantItem &constant) override
  {
    std::string identifier
      = base_path.as_string () + "::" + constant.get_identifier ();
    resolver->get_name_scope ().insert (
      identifier, constant.get_node_id (), constant.get_locus (), false,
      [&] (std::string, NodeId, Location locus) -> void {
	rust_error_at (constant.get_locus (), "redefined multiple times");
	rust_error_at (locus, "was defined here");
      });
    resolver->insert_new_definition (constant.get_node_id (),
				     Definition{constant.get_node_id (),
						constant.get_node_id ()});
  }

  void visit (AST::Function &function) override
  {
    std::string identifier
      = base_path.as_string () + "::" + function.get_function_name ();
    resolver->get_name_scope ().insert (
      identifier, function.get_node_id (), function.get_locus (), false,
      [&] (std::string, NodeId, Location locus) -> void {
	rust_error_at (function.get_locus (), "redefined multiple times");
	rust_error_at (locus, "was defined here");
      });
    resolver->insert_new_definition (function.get_node_id (),
				     Definition{function.get_node_id (),
						function.get_node_id ()});
  }

  void visit (AST::Method &method) override
  {
    std::string identifier
      = base_path.as_string () + "::" + method.get_method_name ();
    resolver->get_name_scope ().insert (
      identifier, method.get_node_id (), method.get_locus (), false,
      [&] (std::string, NodeId, Location locus) -> void {
	rust_error_at (method.get_locus (), "redefined multiple times");
	rust_error_at (locus, "was defined here");
      });
    resolver->insert_new_definition (method.get_node_id (),
				     Definition{method.get_node_id (),
						method.get_node_id ()});
  }

private:
  ResolveToplevelImplItem (AST::Type *base)
    : ResolverBase (UNKNOWN_NODEID), base (base),
      base_path (AST::SimplePath::create_empty ())
  {
    ResolveTypeToSimplePath::go (base, base_path, true);
  }

  AST::Type *base;
  AST::SimplePath base_path;
};

} // namespace Resolver
} // namespace Rust

#endif // RUST_AST_RESOLVE_IMPLITEM_H
