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

// rust-ast-resolve-expr.h

void
ResolvePath::resolve_path (AST::PathInExpression *expr)
{
  // resolve root segment first then apply segments in turn
  std::vector<AST::PathExprSegment> &segs = expr->get_segments ();
  AST::PathExprSegment &root_segment = segs.at (0);
  AST::PathIdentSegment &root_ident_seg = root_segment.get_ident_segment ();

  bool segment_is_type = false;
  CanonicalPath root_seg_path
    = CanonicalPath::new_seg (root_segment.get_node_id (),
			      root_ident_seg.as_string ());

  // name scope first
  if (resolver->get_name_scope ().lookup (root_seg_path, &resolved_node))
    {
      segment_is_type = false;
      resolver->insert_resolved_name (root_segment.get_node_id (),
				      resolved_node);
      resolver->insert_new_definition (root_segment.get_node_id (),
				       Definition{expr->get_node_id (),
						  parent});
    }
  // check the type scope
  else if (resolver->get_type_scope ().lookup (root_seg_path, &resolved_node))
    {
      segment_is_type = true;
      resolver->insert_resolved_type (root_segment.get_node_id (),
				      resolved_node);
      resolver->insert_new_definition (root_segment.get_node_id (),
				       Definition{expr->get_node_id (),
						  parent});
    }
  else
    {
      rust_error_at (expr->get_locus (),
		     "Cannot find path %<%s%> in this scope",
		     root_segment.as_string ().c_str ());
      return;
    }

  if (root_segment.has_generic_args ())
    {
      bool ok = ResolveTypeToCanonicalPath::type_resolve_generic_args (
	root_segment.get_generic_args ());
      if (!ok)
	{
	  rust_error_at (root_segment.get_locus (),
			 "failed to resolve generic arguments");
	  return;
	}
    }

  bool is_single_segment = segs.size () == 1;
  if (is_single_segment)
    {
      if (segment_is_type)
	resolver->insert_resolved_type (expr->get_node_id (), resolved_node);
      else
	resolver->insert_resolved_name (expr->get_node_id (), resolved_node);

      resolver->insert_new_definition (expr->get_node_id (),
				       Definition{expr->get_node_id (),
						  parent});
      return;
    }

  resolve_segments (root_seg_path, 1, expr->get_segments (),
		    expr->get_node_id (), expr->get_locus ());
}

void
ResolvePath::resolve_path (AST::QualifiedPathInExpression *expr)
{
  AST::QualifiedPathType &root_segment = expr->get_qualified_path_type ();

  bool canonicalize_type_with_generics = false;
  ResolveType::go (&root_segment.get_as_type_path (),
		   root_segment.get_node_id (),
		   canonicalize_type_with_generics);

  ResolveType::go (root_segment.get_type ().get (), root_segment.get_node_id (),
		   canonicalize_type_with_generics);

  bool type_resolve_generic_args = true;
  CanonicalPath impl_type_seg
    = ResolveTypeToCanonicalPath::resolve (*root_segment.get_type ().get (),
					   canonicalize_type_with_generics,
					   type_resolve_generic_args);

  CanonicalPath trait_type_seg
    = ResolveTypeToCanonicalPath::resolve (root_segment.get_as_type_path (),
					   canonicalize_type_with_generics,
					   type_resolve_generic_args);
  CanonicalPath root_seg_path
    = TraitImplProjection::resolve (root_segment.get_node_id (), trait_type_seg,
				    impl_type_seg);
  bool segment_is_type = false;

  // name scope first
  if (resolver->get_name_scope ().lookup (root_seg_path, &resolved_node))
    {
      segment_is_type = false;
      resolver->insert_resolved_name (root_segment.get_node_id (),
				      resolved_node);
      resolver->insert_new_definition (root_segment.get_node_id (),
				       Definition{expr->get_node_id (),
						  parent});
    }
  // check the type scope
  else if (resolver->get_type_scope ().lookup (root_seg_path, &resolved_node))
    {
      segment_is_type = true;
      resolver->insert_resolved_type (root_segment.get_node_id (),
				      resolved_node);
      resolver->insert_new_definition (root_segment.get_node_id (),
				       Definition{expr->get_node_id (),
						  parent});
    }
  else
    {
      rust_error_at (expr->get_locus (),
		     "Cannot find path %<%s%> in this scope",
		     root_segment.as_string ().c_str ());
      return;
    }

  bool is_single_segment = expr->get_segments ().empty ();
  if (is_single_segment)
    {
      if (segment_is_type)
	resolver->insert_resolved_type (expr->get_node_id (), resolved_node);
      else
	resolver->insert_resolved_name (expr->get_node_id (), resolved_node);

      resolver->insert_new_definition (expr->get_node_id (),
				       Definition{expr->get_node_id (),
						  parent});
      return;
    }

  resolve_segments (root_seg_path, 0, expr->get_segments (),
		    expr->get_node_id (), expr->get_locus ());
}

void
ResolvePath::resolve_segments (CanonicalPath prefix, size_t offs,
			       std::vector<AST::PathExprSegment> &segs,
			       NodeId expr_node_id, Location expr_locus)
{
  // we can attempt to resolve this path fully
  CanonicalPath path = prefix;
  bool segment_is_type = false;
  for (size_t i = offs; i < segs.size (); i++)
    {
      AST::PathExprSegment &seg = segs.at (i);
      auto s = ResolvePathSegmentToCanonicalPath::resolve (seg);
      path = path.append (s);

      // reset state
      segment_is_type = false;
      resolved_node = UNKNOWN_NODEID;

      if (resolver->get_name_scope ().lookup (path, &resolved_node))
	{
	  resolver->insert_resolved_name (seg.get_node_id (), resolved_node);
	  resolver->insert_new_definition (seg.get_node_id (),
					   Definition{expr_node_id, parent});
	}
      // check the type scope
      else if (resolver->get_type_scope ().lookup (path, &resolved_node))
	{
	  segment_is_type = true;
	  resolver->insert_resolved_type (seg.get_node_id (), resolved_node);
	  resolver->insert_new_definition (seg.get_node_id (),
					   Definition{expr_node_id, parent});
	}
      else
	{
	  // attempt to fully resolve the path which is allowed to fail given
	  // the following scenario
	  //
	  // https://github.com/Rust-GCC/gccrs/issues/355 Paths are
	  // resolved fully here, there are limitations though imagine:
	  //
	  // struct Foo<A> (A);
	  //
	  // impl Foo<isize> {
	  //    fn test() -> ...
	  //
	  // impl Foo<f32> {
	  //    fn test() -> ...
	  //
	  // fn main() {
	  //    let a:i32 = Foo::test();
	  //
	  // there are multiple paths that test can resolve to Foo::<?>::test
	  // here so we cannot resolve this case
	  //
	  // canonical names:
	  //
	  // struct Foo<A>            -> Foo
	  // impl Foo<isize>::fn test -> Foo::isize::test
	  // impl Foo<f32>::fn test   -> Foo::f32::test
	  //
	  // Since there is the case we have the following paths for test:
	  //
	  // Foo::isize::test
	  // Foo::f32::test
	  // vs
	  // Foo::test
	  //
	  // but the lookup was simply Foo::test we must rely on type resolution
	  // to figure this type out in a similar fashion to method resolution
	  // with a probe phase

	  // nothing more we can do we need the type resolver to try and resolve
	  // this
	  return;
	}
    }

  // its fully resolved lets mark it as such
  if (resolved_node != UNKNOWN_NODEID)
    {
      if (segment_is_type)
	resolver->insert_resolved_type (expr_node_id, resolved_node);
      else
	resolver->insert_resolved_name (expr_node_id, resolved_node);

      resolver->insert_new_definition (expr_node_id,
				       Definition{expr_node_id, parent});
    }
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
