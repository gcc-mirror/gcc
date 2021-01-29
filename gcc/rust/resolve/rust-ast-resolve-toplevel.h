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

#ifndef RUST_AST_RESOLVE_TOPLEVEL_H
#define RUST_AST_RESOLVE_TOPLEVEL_H

#include "rust-ast-resolve-base.h"
#include "rust-ast-resolve-type.h"
#include "rust-ast-resolve-implitem.h"
#include "rust-ast-full.h"

namespace Rust {
namespace Resolver {

class ResolveTopLevel : public ResolverBase
{
public:
  static void go (AST::Item *item)
  {
    ResolveTopLevel resolver;
    item->accept_vis (resolver);
  };

  void visit (AST::TupleStruct &struct_decl)
  {
    resolver->get_type_scope ().insert (struct_decl.get_identifier (),
					struct_decl.get_node_id (),
					struct_decl.get_locus ());
  }

  void visit (AST::StructStruct &struct_decl)
  {
    resolver->get_type_scope ().insert (struct_decl.get_identifier (),
					struct_decl.get_node_id (),
					struct_decl.get_locus ());
  }

  void visit (AST::StaticItem &var)
  {
    resolver->get_name_scope ().insert (var.get_identifier (),
					var.get_node_id (), var.get_locus ());
    resolver->insert_new_definition (var.get_node_id (),
				     Definition{var.get_node_id (),
						var.get_node_id ()});
    resolver->mark_decl_mutability (var.get_node_id (), var.is_mutable ());
  }

  void visit (AST::ConstantItem &constant)
  {
    resolver->get_name_scope ().insert (constant.get_identifier (),
					constant.get_node_id (),
					constant.get_locus ());
    resolver->insert_new_definition (constant.get_node_id (),
				     Definition{constant.get_node_id (),
						constant.get_node_id ()});
  }

  void visit (AST::Function &function)
  {
    resolver->get_name_scope ().insert (function.get_function_name (),
					function.get_node_id (),
					function.get_locus ());
    resolver->insert_new_definition (function.get_node_id (),
				     Definition{function.get_node_id (),
						function.get_node_id ()});

    // if this does not get a reference it will be determined to be unused
    // lets give it a fake reference to itself
    if (function.get_function_name ().compare ("main") == 0)
      {
	resolver->insert_resolved_name (function.get_node_id (),
					function.get_node_id ());
      }
  }

  void visit (AST::InherentImpl &impl_block)
  {
    if (!ResolveType::go (impl_block.get_type ().get (),
			  impl_block.get_node_id ()))
      return;

    for (auto &impl_item : impl_block.get_impl_items ())
      ResolveToplevelImplItem::go (impl_item.get (),
				   impl_block.get_type ().get ());
  }

private:
  ResolveTopLevel () : ResolverBase (UNKNOWN_NODEID) {}
};

} // namespace Resolver
} // namespace Rust

#endif // RUST_AST_RESOLVE_TOPLEVEL_H
