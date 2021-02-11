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

#ifndef RUST_AST_RESOLVE_ITEM_H
#define RUST_AST_RESOLVE_ITEM_H

#include "rust-ast-resolve-base.h"
#include "rust-ast-full.h"
#include "rust-ast-resolve-type.h"
#include "rust-ast-resolve-pattern.h"
#include "rust-ast-resolve-stmt.h"

namespace Rust {
namespace Resolver {

class ResolveItem : public ResolverBase
{
public:
  static void go (AST::Item *item)
  {
    ResolveItem resolver;
    item->accept_vis (resolver);
  };

  ~ResolveItem () {}

  void visit (AST::TupleStruct &struct_decl)
  {
    struct_decl.iterate ([&] (AST::TupleField &field) mutable -> bool {
      ResolveType::go (field.get_field_type ().get (),
		       struct_decl.get_node_id ());
      return true;
    });
  }

  void visit (AST::StructStruct &struct_decl)
  {
    struct_decl.iterate ([&] (AST::StructField &field) mutable -> bool {
      ResolveType::go (field.get_field_type ().get (),
		       struct_decl.get_node_id ());
      return true;
    });
  }

  void visit (AST::StaticItem &var)
  {
    ResolveType::go (var.get_type ().get (), var.get_node_id ());
    ResolveExpr::go (var.get_expr ().get (), var.get_node_id ());

    // the mutability checker needs to verify for immutable decls the number
    // of assignments are <1. This marks an implicit assignment
    resolver->mark_assignment_to_decl (var.get_node_id (), var.get_node_id ());
  }

  void visit (AST::ConstantItem &constant)
  {
    ResolveType::go (constant.get_type ().get (), constant.get_node_id ());
    ResolveExpr::go (constant.get_expr ().get (), constant.get_node_id ());

    // the mutability checker needs to verify for immutable decls the number
    // of assignments are <1. This marks an implicit assignment
    resolver->mark_decl_mutability (constant.get_node_id (), false);
    resolver->mark_assignment_to_decl (constant.get_node_id (),
				       constant.get_node_id ());
  }

  void visit (AST::Function &function)
  {
    if (function.has_return_type ())
      ResolveType::go (function.get_return_type ().get (),
		       function.get_node_id ());

    NodeId scope_node_id = function.get_node_id ();
    resolver->get_name_scope ().push (scope_node_id);
    resolver->get_type_scope ().push (scope_node_id);
    resolver->get_label_scope ().push (scope_node_id);
    resolver->push_new_name_rib (resolver->get_name_scope ().peek ());
    resolver->push_new_type_rib (resolver->get_type_scope ().peek ());
    resolver->push_new_label_rib (resolver->get_type_scope ().peek ());

    // we make a new scope so the names of parameters are resolved and shadowed
    // correctly
    for (auto &param : function.get_function_params ())
      {
	ResolveType::go (param.get_type ().get (), param.get_node_id ());
	PatternDeclaration::go (param.get_pattern ().get (),
				param.get_node_id ());

	// the mutability checker needs to verify for immutable decls the number
	// of assignments are <1. This marks an implicit assignment
	resolver->mark_assignment_to_decl (param.get_pattern ()->get_node_id (),
					   param.get_node_id ());
      }

    // resolve the function body
    ResolveExpr::go (function.get_definition ().get (),
		     function.get_node_id ());

    resolver->get_name_scope ().pop ();
    resolver->get_type_scope ().pop ();
    resolver->get_label_scope ().pop ();
  }

  void visit (AST::InherentImpl &impl_block)
  {
    NodeId resolved_node = ResolveType::go (impl_block.get_type ().get (),
					    impl_block.get_node_id ());
    if (resolved_node == UNKNOWN_NODEID)
      return;

    resolver->get_type_scope ().insert (
      "Self", resolved_node, impl_block.get_type ()->get_locus_slow ());

    for (auto &impl_item : impl_block.get_impl_items ())
      impl_item->accept_vis (*this);

    resolver->get_type_scope ().peek ()->clear_name ("Self", resolved_node);
  }

  void visit (AST::Method &method)
  {
    if (method.has_return_type ())
      ResolveType::go (method.get_return_type ().get (), method.get_node_id ());

    NodeId scope_node_id = method.get_node_id ();
    resolver->get_name_scope ().push (scope_node_id);
    resolver->get_type_scope ().push (scope_node_id);
    resolver->push_new_name_rib (resolver->get_name_scope ().peek ());
    resolver->push_new_type_rib (resolver->get_type_scope ().peek ());

    // self turns into self: Self as a function param
    AST::SelfParam &self_param = method.get_self_param ();
    AST::IdentifierPattern self_pattern (
      self_param.get_node_id (), "self", self_param.get_locus (),
      self_param.get_has_ref (), self_param.get_is_mut (),
      std::unique_ptr<AST::Pattern> (nullptr));

    std::vector<std::unique_ptr<AST::TypePathSegment> > segments;
    segments.push_back (std::unique_ptr<AST::TypePathSegment> (
      new AST::TypePathSegment ("Self", false, self_param.get_locus ())));

    AST::TypePath self_type_path (std::move (segments),
				  self_param.get_locus ());

    ResolveType::go (&self_type_path, self_param.get_node_id ());
    PatternDeclaration::go (&self_pattern, self_param.get_node_id ());

    resolver->mark_assignment_to_decl (self_pattern.get_node_id (),
				       self_pattern.get_node_id ());

    // we make a new scope so the names of parameters are resolved and shadowed
    // correctly
    for (auto &param : method.get_function_params ())
      {
	ResolveType::go (param.get_type ().get (), param.get_node_id ());
	PatternDeclaration::go (param.get_pattern ().get (),
				param.get_node_id ());

	// the mutability checker needs to verify for immutable decls the number
	// of assignments are <1. This marks an implicit assignment
	resolver->mark_assignment_to_decl (param.get_pattern ()->get_node_id (),
					   param.get_node_id ());
      }

    // resolve the function body
    ResolveExpr::go (method.get_definition ().get (), method.get_node_id ());

    resolver->get_name_scope ().pop ();
    resolver->get_type_scope ().pop ();
  }

private:
  ResolveItem () : ResolverBase (UNKNOWN_NODEID) {}
};

} // namespace Resolver
} // namespace Rust

#endif // RUST_AST_RESOLVE_ITEM_H
