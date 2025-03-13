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
#include "rust-rib.h"
#include "rust-toplevel-name-resolver-2.0.h"

namespace Rust {
namespace Resolver2_0 {

class Early : public DefaultResolver
{
  using DefaultResolver::visit;

  TopLevel toplevel;
  bool dirty;

public:
  Early (NameResolutionContext &ctx);

  bool is_dirty () { return dirty; }

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
  void visit (AST::UseDeclaration &) override;

  struct ImportData
  {
    enum class Kind
    {
      Simple,
      Glob,
      Rebind
    } kind;

    static ImportData
    Simple (std::vector<std::pair<Rib::Definition, Namespace>> &&definitions)
    {
      return ImportData (Kind::Simple, std::move (definitions));
    }

    static ImportData
    Rebind (std::vector<std::pair<Rib::Definition, Namespace>> &&definitions)
    {
      return ImportData (Kind::Rebind, std::move (definitions));
    }

    static ImportData Glob (Rib::Definition module)
    {
      return ImportData (Kind::Glob, module);
    }

    Rib::Definition module () const
    {
      rust_assert (kind == Kind::Glob);
      return glob_module;
    }

    std::vector<std::pair<Rib::Definition, Namespace>> definitions () const
    {
      rust_assert (kind != Kind::Glob);
      return std::move (resolved_definitions);
    }

  private:
    ImportData (
      Kind kind,
      std::vector<std::pair<Rib::Definition, Namespace>> &&definitions)
      : kind (kind), resolved_definitions (std::move (definitions))
    {}

    ImportData (Kind kind, Rib::Definition module)
      : kind (kind), glob_module (module)
    {}

    // TODO: Should this be a union?

    // For Simple and Rebind
    std::vector<std::pair<Rib::Definition, Namespace>> resolved_definitions;

    // For Glob
    Rib::Definition glob_module;
  };

  struct ImportPair
  {
    TopLevel::ImportKind import_kind;
    ImportData data;

    explicit ImportPair (TopLevel::ImportKind &&kind, ImportData &&data)
      : import_kind (std::move (kind)), data (std::move (data))
    {}
  };

  class ImportMappings
  {
  public:
    std::vector<ImportPair> &new_or_access (NodeId path_id)
    {
      // We insert an empty vector, unless an element was already present for
      // `use_dec_id` - which is returned in the tuple's first member
      auto iter = mappings.insert ({{path_id}, {}});

      // We then get that tuple's first member, which will be an iterator to the
      // existing vec<pair<ImportKind, ImportData>> OR an iterator to our newly
      // created empty vector (plus its key since this is a hashmap iterator).
      // we then access the second member of the pair to get access to the
      // vector directly.
      return iter.first->second;
    }

    void insert (NodeId path_id, std::vector<ImportPair> &&pairs)
    {
      mappings.insert ({{path_id}, std::move (pairs)});
    }

    // Same as `insert`, but with just one node
    void insert (NodeId path_id, ImportPair &&pair)
    {
      mappings.insert ({{path_id}, {pair}});
    }

    std::vector<ImportPair> &get (NodeId use_id) { return mappings[use_id]; }

  private:
    // Each path can import in multiple namespaces, hence the mapping from one
    // path to a vector of import pairs
    std::unordered_map<NodeId, std::vector<ImportPair>> mappings;
  };

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

  // Mappings between an import and the definition it imports
  ImportMappings import_mappings;

  // FIXME: Documentation
  // Call this on all the paths of a UseDec - so each flattened path in a
  // UseTreeList for example
  // FIXME: Should that return `found`?
  bool resolve_simple_import (NodeId use_dec_id, TopLevel::ImportKind &&import);
  bool resolve_glob_import (NodeId use_dec_id, TopLevel::ImportKind &&import);
  bool resolve_rebind_import (NodeId use_dec_id, TopLevel::ImportKind &&import);

  template <typename P>
  std::vector<std::pair<Rib::Definition, Namespace>>
  resolve_path_in_all_ns (const P &path)
  {
    const auto &segments = path.get_segments ();
    std::vector<std::pair<Rib::Definition, Namespace>> resolved;

    // Pair a definition with the namespace it was found in
    auto pair_with_ns = [&] (Namespace ns) {
      return [&, ns] (Rib::Definition def) {
	auto pair = std::make_pair (def, ns);
	return resolved.emplace_back (std::move (pair));
      };
    };

    ctx.resolve_path (segments, Namespace::Values)
      .map (pair_with_ns (Namespace::Values));
    ctx.resolve_path (segments, Namespace::Types)
      .map (pair_with_ns (Namespace::Types));
    ctx.resolve_path (segments, Namespace::Macros)
      .map (pair_with_ns (Namespace::Macros));

    return resolved;
  }

  // Handle an import, resolving it to its definition and adding it to the list
  // of import mappings
  void build_import_mapping (
    std::pair<NodeId, std::vector<TopLevel::ImportKind>> &&use_import);

  TextualScope textual_scope;
  std::vector<Error> macro_resolve_errors;

  void collect_error (Error e) { macro_resolve_errors.push_back (e); }

  void finalize_simple_import (const Early::ImportPair &mapping);

  void finalize_glob_import (NameResolutionContext &ctx,
			     const Early::ImportPair &mapping);

  void finalize_rebind_import (const Early::ImportPair &mapping);
};

} // namespace Resolver2_0
} // namespace Rust

#endif // ! RUST_EARLY_NAME_RESOLVER_2_0_H
