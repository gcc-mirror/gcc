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

#include "rust-ast-resolve.h"
#include "rust-ast-full.h"
#include "rust-tyty.h"
#include "rust-ast-resolve-toplevel.h"
#include "rust-ast-resolve-item.h"
#include "rust-ast-resolve-expr.h"
#include "rust-ast-resolve-struct-expr-field.h"

extern bool
saw_errors (void);

namespace Rust {
namespace Resolver {

// NameResolution

NameResolution *
NameResolution::get ()
{
  static NameResolution *instance;
  if (instance == nullptr)
    instance = new NameResolution ();

  return instance;
}

NameResolution::NameResolution ()
  : resolver (Resolver::get ()), mappings (Analysis::Mappings::get ())
{
  // these are global
  resolver->get_type_scope ().push (mappings->get_next_node_id ());
  resolver->insert_builtin_types (resolver->get_type_scope ().peek ());
  resolver->push_new_type_rib (resolver->get_type_scope ().peek ());
}

void
NameResolution::Resolve (AST::Crate &crate)
{
  auto resolver = get ();
  resolver->go (crate);
}

void
NameResolution::go (AST::Crate &crate)
{
  // lookup current crate name
  CrateNum cnum = mappings->get_current_crate ();
  std::string crate_name;
  bool ok = mappings->get_crate_name (cnum, crate_name);
  rust_assert (ok);

  // setup the ribs
  NodeId scope_node_id = crate.get_node_id ();
  resolver->get_name_scope ().push (scope_node_id);
  resolver->get_type_scope ().push (scope_node_id);
  resolver->get_label_scope ().push (scope_node_id);
  resolver->push_new_name_rib (resolver->get_name_scope ().peek ());
  resolver->push_new_type_rib (resolver->get_type_scope ().peek ());
  resolver->push_new_label_rib (resolver->get_type_scope ().peek ());

  // get the root segment
  CanonicalPath crate_prefix
    = CanonicalPath::new_seg (scope_node_id, crate_name);
  crate_prefix.set_crate_num (cnum);

  // first gather the top-level namespace names then we drill down so this
  // allows for resolving forward declarations since an impl block might have
  // a Self type Foo which is defined after the impl block for example.
  for (auto it = crate.items.begin (); it != crate.items.end (); it++)
    ResolveTopLevel::go (it->get (), CanonicalPath::create_empty (),
			 crate_prefix);

  // FIXME remove this
  if (saw_errors ())
    return;

  // next we can drill down into the items and their scopes
  for (auto it = crate.items.begin (); it != crate.items.end (); it++)
    ResolveItem::go (it->get (), CanonicalPath::create_empty (), crate_prefix);
}

// rust-ast-resolve-struct-expr-field.h

void
ResolveStructExprField::visit (AST::StructExprFieldIdentifierValue &field)
{
  ResolveExpr::go (field.get_value ().get (), field.get_node_id (), prefix,
		   canonical_prefix);
}

void
ResolveStructExprField::visit (AST::StructExprFieldIndexValue &field)
{
  ResolveExpr::go (field.get_value ().get (), field.get_node_id (), prefix,
		   canonical_prefix);
}

void
ResolveStructExprField::visit (AST::StructExprFieldIdentifier &field)
{
  AST::IdentifierExpr expr (field.get_field_name (), {}, field.get_locus ());
  expr.set_node_id (field.get_node_id ());

  ResolveExpr::go (&expr, field.get_node_id (), prefix, canonical_prefix);
}

// rust-ast-resolve-item.h

void
ResolveItem::resolve_impl_item (AST::TraitImplItem *item,
				const CanonicalPath &prefix,
				const CanonicalPath &canonical_prefix)
{
  ResolveImplItems::go (item, prefix, canonical_prefix);
}

void
ResolveItem::resolve_impl_item (AST::InherentImplItem *item,
				const CanonicalPath &prefix,
				const CanonicalPath &canonical_prefix)
{
  ResolveImplItems::go (item, prefix, canonical_prefix);
}

void
ResolveItem::resolve_extern_item (AST::ExternalItem *item)
{
  ResolveExternItem::go (item);
}

// qualified path in type

bool
ResolveRelativeTypePath::resolve_qual_seg (AST::QualifiedPathType &seg,
					   CanonicalPath &result)
{
  if (seg.is_error ())
    {
      rust_error_at (seg.get_locus (), "segment has error: %s",
		     seg.as_string ().c_str ());
      return false;
    }
  bool include_generic_args_in_path = false;

  NodeId type_resolved_node
    = ResolveType::go (seg.get_type ().get (), seg.get_node_id ());
  if (type_resolved_node == UNKNOWN_NODEID)
    return false;

  CanonicalPath impl_type_seg
    = ResolveTypeToCanonicalPath::resolve (*seg.get_type ().get (),
					   include_generic_args_in_path);
  if (!seg.has_as_clause ())
    {
      result = result.append (impl_type_seg);
      return true;
    }

  NodeId trait_resolved_node
    = ResolveType::go (&seg.get_as_type_path (), seg.get_node_id ());
  if (trait_resolved_node == UNKNOWN_NODEID)
    return false;

  CanonicalPath trait_type_seg
    = ResolveTypeToCanonicalPath::resolve (seg.get_as_type_path (),
					   include_generic_args_in_path);
  CanonicalPath projection
    = TraitImplProjection::resolve (seg.get_node_id (), trait_type_seg,
				    impl_type_seg);
  result = result.append (projection);
  return true;
}

} // namespace Resolver
} // namespace Rust
