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

#ifndef RUST_RIB_H
#define RUST_RIB_H

#include "rust-system.h"
#include "rust-ast.h"
#include "optional.h"
#include "expected.h"

namespace Rust {
namespace Resolver2_0 {

/**

pub enum Namespace {
   /// The type namespace includes `struct`s, `enum`s, `union`s, `trait`s, and
`mod`s
   /// (and, by extension, crates).
   ///
   /// Note that the type namespace includes other items; this is not an
   /// exhaustive list.
   TypeNS,
   /// The value namespace includes `fn`s, `const`s, `static`s, and local
variables (including function arguments). ValueNS,
   /// The macro namespace includes `macro_rules!` macros, declarative `macro`s,
   /// procedural macros, attribute macros, `derive` macros, and non-macro
attributes
   /// like `#[inline]` and `#[rustfmt::skip]`.
   MacroNS,
}

*/

// FIXME: There's no `labels` namespace, not sure if we need one or how to keep
// one
// FIXME: And where are things like loop labels kept?

/**
 * All namespaces that Rust's name resolution needs to handle
 */
// TODO: Move to `rust-forever-stack.h`?
enum class Namespace
{
  Values,
  Types,
  Labels,
  Macros,
  // TODO: Which namespaces are we missing?
};

/**
 * Error returned by `Rib::insert` when the key was already present in the Rib's
 * map. The class contains the previously-inserted NodeId as well as the name of
 * the node.
 */
struct DuplicateNameError
{
  // TODO: We might need multiple kinds of errors later down the line
  DuplicateNameError (std::string name, NodeId existing);

  std::string name;
  NodeId existing;
};

/**
 * A rib is a container of nodes, either declaration or usages, as well as the
 * identifier each node uses. They are used to delimit lexical scopes, and have
 * an impact on name resolution - they restrict certain name accesses and serve
 * as boundaries between scopes.

 * For example, if we are resolving the following *variable* use:
 *
 * ```rust
 * fn outer() {
 *     let a = 15; // decl
 *     fn inner() -> i32 {
 *         a // use
 *     }
 * }
 * ```
 *
 * The `Function` rib we will have pushed will restrict the access to `outer`'s
 * `a` declaration: Variable uses cannot cross function boundaries. On the other
 * hand, if we were resolving a type usage, this would be perfectly allowed.
 */
class Rib
{
public:
  // TODO: Rename the class? to what? Binding? Declaration?
  // This is useful for items which are in namespaces where shadowing is not
  // allowed, but which are still shadowable! for example, when you do a glob
  // import, if a later import has the same name as an item imported in the glob
  // import, that glob imported item will need to get shadowed
  class Definition
  {
  public:
    static Definition NonShadowable (NodeId id, bool enum_variant = false);
    static Definition Shadowable (NodeId id);
    static Definition Globbed (NodeId id);

    // checked shadowable -> non_shadowable -> globbed
    // we have shadowable *and* globbed in order to control
    // resolution priority
    // we *could* use a single vector with 2 indices here
    // but it's probably not worth it for now
    std::vector<NodeId> ids_shadowable;
    std::vector<NodeId> ids_non_shadowable;
    std::vector<NodeId> ids_globbed;

    // Enum variant should be skipped when dealing with inner definition.
    // struct E2;
    //
    // enum MyEnum<T> /* <-- Should be kept */{
    //     E2 /* <-- Should be skipped */ (E2);
    // }
    bool enum_variant;

    Definition () = default;

    Definition &operator= (const Definition &) = default;
    Definition (Definition const &) = default;

    bool is_variant () const;

    bool is_ambiguous () const;

    NodeId get_node_id () const
    {
      if (!ids_shadowable.empty ())
	return ids_shadowable.back ();

      rust_assert (!is_ambiguous ());

      if (!ids_non_shadowable.empty ())
	return ids_non_shadowable.back ();

      rust_assert (!ids_globbed.empty ());
      return ids_globbed.back ();
    }

    std::string to_string () const;

  private:
    enum class Mode
    {
      SHADOWABLE,
      NON_SHADOWABLE,
      GLOBBED
    };

    Definition (NodeId id, Mode mode, bool enum_variant);
  };

  enum class Kind
  {
    Normal,
    Module,
    Function,
    ConstantItem, // -> this variant has a boolean
    TraitOrImpl,
    /* Any item other than a Module, Function, Constant, Trait or Impl block */
    Item,
    Closure,
    MacroDefinition,
    /* Ban the use of forward-declared generic parameters in defaults */
    ForwardTypeParamBan,
    /* Const generic, as in the following example: fn foo<T, const X: T>() {} */
    ConstParamType,
    /* Prelude rib, used for both the language prelude (i32,usize,etc) and the
     * (future) {std,core}::prelude::* import. A regular rib with the
     * restriction that you cannot `use` items from the Prelude
     */
    Prelude,
  } kind;

  static std::string kind_to_string (Rib::Kind kind)
  {
    switch (kind)
      {
      case Rib::Kind::Normal:
	return "Normal";
      case Rib::Kind::Module:
	return "Module";
      case Rib::Kind::Function:
	return "Function";
      case Rib::Kind::ConstantItem:
	return "ConstantItem";
      case Rib::Kind::TraitOrImpl:
	return "TraitOrImpl";
      case Rib::Kind::Item:
	return "Item";
      case Rib::Kind::Closure:
	return "Closure";
      case Rib::Kind::MacroDefinition:
	return "Macro definition";
      case Rib::Kind::ForwardTypeParamBan:
	return "Forward type param ban";
      case Rib::Kind::ConstParamType:
	return "Const Param Type";
      default:
	rust_unreachable ();
      }
  }

  Rib (Kind kind);
  Rib (Kind kind, std::string identifier, NodeId id);
  Rib (Kind kind, std::unordered_map<std::string, NodeId> values);

  // TODO: What's the correctbehavior if the key already exists? What if a decl
  // and use are in the same rib? Is that possible? Okay based on RibKind?

  /**
   * Insert a new node in the rib
   *
   * @param name The name associated with the AST node
   * @param def The `Definition` to insert
   *
   * @return `DuplicateNameError` if the node is already present in the rib. The
   *         `DuplicateNameError` class contains the NodeId of the existing
   * node. Returns the new NodeId on success.
   */
  tl::expected<NodeId, DuplicateNameError> insert (std::string name,
						   Definition def);

  /**
   * Access an inserted NodeId.
   *
   * @return tl::nullopt if the key does not exist, the NodeId otherwise
   */
  tl::optional<Rib::Definition> get (const std::string &name);

  /* View all the values stored in the rib */
  const std::unordered_map<std::string, Definition> &get_values () const;

private:
  // TODO: Switch this to (NodeId, shadowable = false);
  std::unordered_map<std::string, Definition> values;
};

} // namespace Resolver2_0
} // namespace Rust

#endif // !RUST_RIB_H
