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

#include "rust-finalize-imports-2.0.h"
#include "rust-default-resolver.h"
#include "rust-hir-map.h"
#include "rust-name-resolution-context.h"
#include "rust-rib.h"
#include "rust-toplevel-name-resolver-2.0.h"

namespace Rust {
namespace Resolver2_0 {

void
GlobbingVisitor::go (AST::Module *module)
{
  for (auto &i : module->get_items ())
    visit (i);
}

void
GlobbingVisitor::visit (AST::Module &module)
{
  if (module.get_visibility ().is_public ())
    ctx.insert_globbed (module.get_name (), module.get_node_id (),
			Namespace::Types);
}

void
GlobbingVisitor::visit (AST::MacroRulesDefinition &macro)
{
  if (macro.get_visibility ().is_public ())
    ctx.insert_globbed (macro.get_rule_name (), macro.get_node_id (),
			Namespace::Macros);
}

void
GlobbingVisitor::visit (AST::Function &function)
{
  if (function.get_visibility ().is_public ())
    ctx.insert_globbed (function.get_function_name (), function.get_node_id (),
			Namespace::Values);
}

void
GlobbingVisitor::visit (AST::StaticItem &static_item)
{
  if (static_item.get_visibility ().is_public ())
    ctx.insert_globbed (static_item.get_identifier (),
			static_item.get_node_id (), Namespace::Values);
}

void
GlobbingVisitor::visit (AST::StructStruct &struct_item)
{
  if (struct_item.get_visibility ().is_public ())
    {
      ctx.insert_globbed (struct_item.get_identifier (),
			  struct_item.get_node_id (), Namespace::Types);
      if (struct_item.is_unit_struct ())
	ctx.insert_globbed (struct_item.get_identifier (),
			    struct_item.get_node_id (), Namespace::Values);
    }
}

void
GlobbingVisitor::visit (AST::TupleStruct &tuple_struct)
{
  if (tuple_struct.get_visibility ().is_public ())
    {
      ctx.insert_globbed (tuple_struct.get_identifier (),
			  tuple_struct.get_node_id (), Namespace::Types);

      ctx.insert_globbed (tuple_struct.get_identifier (),
			  tuple_struct.get_node_id (), Namespace::Values);
    }
}

void
GlobbingVisitor::visit (AST::Enum &enum_item)
{
  if (enum_item.get_visibility ().is_public ())
    ctx.insert_globbed (enum_item.get_identifier (), enum_item.get_node_id (),
			Namespace::Types);
}

void
GlobbingVisitor::visit (AST::Union &union_item)
{
  if (union_item.get_visibility ().is_public ())
    ctx.insert_globbed (union_item.get_identifier (), union_item.get_node_id (),
			Namespace::Values);
}

void
GlobbingVisitor::visit (AST::ConstantItem &const_item)
{
  if (const_item.get_visibility ().is_public ())
    ctx.insert_globbed (const_item.get_identifier (), const_item.get_node_id (),
			Namespace::Values);
}

void
GlobbingVisitor::visit (AST::ExternCrate &crate)
{}

void
GlobbingVisitor::visit (AST::UseDeclaration &use)
{
  // Handle cycles ?
}

} // namespace Resolver2_0
} // namespace Rust
