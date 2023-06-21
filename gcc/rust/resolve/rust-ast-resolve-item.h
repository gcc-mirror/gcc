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

#ifndef RUST_AST_RESOLVE_ITEM_H
#define RUST_AST_RESOLVE_ITEM_H

#include "rust-ast-full-decls.h"
#include "rust-ast-resolve-base.h"

#include "config.h"

namespace Rust {
namespace Resolver {

class ResolveTraitItems : public ResolverBase
{
  using Rust::Resolver::ResolverBase::visit;

public:
  static void go (AST::AssociatedItem *item, const CanonicalPath &prefix,
		  const CanonicalPath &canonical_prefix);

  void visit (AST::Function &type) override;
  void visit (AST::TraitItemType &type) override;
  void visit (AST::TraitItemConst &constant) override;

private:
  ResolveTraitItems (const CanonicalPath &prefix,
		     const CanonicalPath &canonical_prefix);

  const CanonicalPath &prefix;
  const CanonicalPath &canonical_prefix;
};

class ResolveItem : public ResolverBase
{
public:
  using Rust::Resolver::ResolverBase::visit;

  static void go (AST::Item *item, const CanonicalPath &prefix,
		  const CanonicalPath &canonical_prefix);

  void visit (AST::TypeAlias &alias) override;
  void visit (AST::Module &module) override;
  void visit (AST::TupleStruct &struct_decl) override;
  void visit (AST::Enum &enum_decl) override;
  /* EnumItem doesn't need to be handled, no fields.  */
  void visit (AST::EnumItem &item) override;
  void visit (AST::EnumItemTuple &item) override;
  void visit (AST::EnumItemStruct &item) override;
  void visit (AST::EnumItemDiscriminant &item) override;
  void visit (AST::StructStruct &struct_decl) override;
  void visit (AST::Union &union_decl) override;
  void visit (AST::StaticItem &var) override;
  void visit (AST::ConstantItem &constant) override;
  void visit (AST::Function &function) override;
  void visit (AST::InherentImpl &impl_block) override;
  void visit (AST::TraitImpl &impl_block) override;
  void visit (AST::Trait &trait) override;
  void visit (AST::ExternBlock &extern_block) override;
  void visit (AST::UseDeclaration &) override;

protected:
  void resolve_impl_item (AST::AssociatedItem *item,
			  const CanonicalPath &prefix,
			  const CanonicalPath &canonical_prefix);
  void resolve_extern_item (AST::ExternalItem *item);

  ResolveItem (const CanonicalPath &prefix,
	       const CanonicalPath &canonical_prefix);

  const CanonicalPath &prefix;
  const CanonicalPath &canonical_prefix;
};

class ResolveImplItems : public ResolveItem
{
  using Rust::Resolver::ResolveItem::visit;

public:
  static void go (AST::AssociatedItem *item, const CanonicalPath &prefix,
		  const CanonicalPath &canonical_prefix);

  void visit (AST::TypeAlias &alias) override;

private:
  ResolveImplItems (const CanonicalPath &prefix,
		    const CanonicalPath &canonical_prefix);
};

class ResolveExternItem : public ResolverBase
{
  using Rust::Resolver::ResolverBase::visit;

public:
  static void go (AST::ExternalItem *item, const CanonicalPath &prefix,
		  const CanonicalPath &canonical_prefix);

  void visit (AST::ExternalFunctionItem &function) override;
  void visit (AST::ExternalStaticItem &item) override;

private:
  ResolveExternItem (const CanonicalPath &prefix,
		     const CanonicalPath &canonical_prefix)
    : ResolverBase (), prefix (prefix), canonical_prefix (canonical_prefix)
  {}

  const CanonicalPath &prefix;
  const CanonicalPath &canonical_prefix;
};

class Import
{
public:
  Import (AST::SimplePath path, bool is_glob, std::string name)
    : path (path), is_glob_f (is_glob), name (name)
  {}

  AST::SimplePath &get_path () { return path; }

  const AST::SimplePath &get_path () const { return path; }

  bool is_glob () const { return is_glob_f; }

  const std::string &get_name () const { return name; }

  void add_prefix (AST::SimplePath prefix);

private:
  AST::SimplePath path;
  bool is_glob_f;
  std::string name;
};

} // namespace Resolver
} // namespace Rust

#if CHECKING_P

namespace selftest {
extern void
rust_simple_path_resolve_test (void);
} // namespace selftest

#endif // CHECKING_P

#endif // RUST_AST_RESOLVE_ITEM_H
