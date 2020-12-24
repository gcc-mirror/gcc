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

  ~ResolveTopLevel () {}

  void visit (AST::ConstantItem &constant)
  {
    resolver->get_name_scope ().insert (constant.get_identifier (),
					constant.get_node_id ());
    resolver->insert_new_definition (constant.get_node_id (),
				     Definition{constant.get_node_id (),
						constant.get_node_id ()});
  }

  void visit (AST::Function &function)
  {
    // function_names are simple std::String identifiers so this can be a
    // NodeId mapping to the Function node
    resolver->get_name_scope ().insert (function.get_function_name (),
					function.get_node_id ());
  }

private:
  ResolveTopLevel () : ResolverBase (UNKNOWN_NODEID) {}
};

} // namespace Resolver
} // namespace Rust

#endif // RUST_AST_RESOLVE_TOPLEVEL_H
