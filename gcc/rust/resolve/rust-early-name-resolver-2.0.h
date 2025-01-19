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

#ifndef RUST_EARLY_NAME_RESOLVER_2_0_H
#define RUST_EARLY_NAME_RESOLVER_2_0_H

#include "optional.h"
#include "rust-ast.h"
#include "rust-ast-visitor.h"
#include "rust-name-resolution-context.h"
#include "rust-default-resolver.h"

namespace Rust {
namespace Resolver2_0 {

class Early : public DefaultResolver
{
  using DefaultResolver::visit;

public:
  Early (NameResolutionContext &ctx);

  void go (AST::Crate &crate);

  const std::vector<Error> &get_macro_resolve_errors () const
  {
    return macro_resolve_errors;
  }

  // we need to handle definitions for textual scoping
  void visit (AST::MacroRulesDefinition &) override;

  // as well as lexical scopes
  void visit (AST::BlockExpr &) override;
  void visit (AST::Module &) override;

  void visit (AST::MacroInvocation &) override;

  void visit (AST::Function &) override;
  void visit (AST::StructStruct &) override;

private:
  void visit_attributes (std::vector<AST::Attribute> &attrs);

  /**
   * Insert a resolved macro invocation into the mappings once, meaning that we
   * can call this function each time the early name resolution pass is underway
   * and it will not trigger assertions for already resolved invocations.
   */
  // TODO: Rename
  void insert_once (AST::MacroInvocation &invocation, NodeId resolved);
  // TODO: Rename
  void insert_once (AST::MacroRulesDefinition &definition);

  /**
   * Macros can either be resolved through textual scoping or regular path
   * scoping - which this class represents. Textual scoping works similarly to a
   * "simple" name resolution algorith, with the addition of "shadowing". Each
   * time a new lexical scope is entered, we push a new map onto the stack, in
   * which newly defined macros are added. The latest defined macro is the one
   * that takes precedence. When resolving a macro invocation to its definition,
   * we walk up the stack and look for a definition in each of the map until we
   * find one. Otherwise, the macro invocation is unresolved, and goes through
   * regular path resolution.
   */
  class TextualScope
  {
  public:
    void push ();
    void pop ();

    void insert (std::string name, NodeId id);
    tl::optional<NodeId> get (const std::string &name);

  private:
    std::vector<std::unordered_map<std::string, NodeId>> scopes;
  };

  TextualScope textual_scope;
  std::vector<Error> macro_resolve_errors;

  void collect_error (Error e) { macro_resolve_errors.push_back (e); }
};

} // namespace Resolver2_0
} // namespace Rust

#endif // ! RUST_EARLY_NAME_RESOLVER_2_0_H
