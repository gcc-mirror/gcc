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

#include "rust-name-resolution-context.h"
#include "optional.h"
#include "rust-mapping-common.h"

namespace Rust {
namespace Resolver2_0 {

BindingLayer::BindingLayer (BindingSource source) : source (source)
{
  push (Binding::Kind::Product);
}

bool
BindingLayer::bind_test (Identifier ident, Binding::Kind kind)
{
  for (auto &bind : bindings)
    {
      if (bind.idents.find (ident.as_string ()) != bind.idents.cend ()
	  && bind.kind == kind)
	{
	  return true;
	}
    }
  return false;
}

void
BindingLayer::push (Binding::Kind kind)
{
  bindings.push_back (Binding (kind));
}

bool
BindingLayer::is_and_bound (Identifier ident)
{
  return bind_test (ident, Binding::Kind::Product);
}

bool
BindingLayer::is_or_bound (Identifier ident)
{
  return bind_test (ident, Binding::Kind::Or);
}

void
BindingLayer::insert_ident (std::string ident, location_t locus, bool is_ref,
			    bool is_mut)
{
  bindings.back ().idents.emplace (
    std::move (ident), std::make_pair (locus, IdentifierMode (is_ref, is_mut)));
}

void
BindingLayer::merge ()
{
  auto last_binding = std::move (bindings.back ());
  bindings.pop_back ();

  if (bindings.back ().has_expected_bindings)
    {
      for (auto &value : bindings.back ().idents)
	{
	  auto ident = value.first;
	  if (last_binding.idents.find (ident) == last_binding.idents.end ())
	    {
	      location_t locus = value.second.first;
	      rust_error_at (locus, ErrorCode::E0408,
			     "variable %qs is not bound in all patterns",
			     ident.c_str ());
	    }
	}
    }

  for (auto &value : last_binding.idents)
    {
      auto res = bindings.back ().idents.emplace (value);
      if (res.second)
	{
	  if (bindings.back ().has_expected_bindings)
	    {
	      auto &ident = value.first;
	      location_t locus = value.second.first;
	      rust_error_at (locus, ErrorCode::E0408,
			     "variable %qs is not bound in all patterns",
			     ident.c_str ());
	    }
	}
      else
	{
	  auto this_mode = value.second.second;
	  auto other_mode = res.first->second.second;
	  if (this_mode != other_mode)
	    {
	      auto &ident = value.first;
	      location_t locus = value.second.first;
	      rust_error_at (locus, ErrorCode::E0409,
			     "variable %qs is bound inconsistently across "
			     "pattern alternatives",
			     ident.c_str ());
	    }
	}
    }

  if (bindings.back ().kind == Binding::Kind::Or)
    bindings.back ().has_expected_bindings = true;
}

BindingSource
BindingLayer::get_source () const
{
  return source;
}

Resolver::CanonicalPath
CanonicalPathRecordCrateRoot::as_path (const NameResolutionContext &)
{
  auto ret = Resolver::CanonicalPath::new_seg (node_id, seg);
  ret.set_crate_num (crate_num);
  return ret;
}

Resolver::CanonicalPath
CanonicalPathRecordNormal::as_path (const NameResolutionContext &ctx)
{
  auto parent_path = get_parent ().as_path (ctx);
  return parent_path.append (Resolver::CanonicalPath::new_seg (node_id, seg));
}

Resolver::CanonicalPath
CanonicalPathRecordLookup::as_path (const NameResolutionContext &ctx)
{
  if (!cache)
    {
      auto res = ctx.lookup (lookup_id).and_then (
	[&ctx] (NodeId id) { return ctx.canonical_ctx.get_record_opt (id); });

      if (!res)
	{
	  // HACK: use a dummy value
	  // this should bring us roughly to parity with nr1.0
	  // since nr1.0 doesn't seem to handle canonical paths for generics
	  //   quite right anyways
	  return Resolver::CanonicalPath::new_seg (UNKNOWN_NODEID, "XXX");
	}

      cache = res.value ();
    }
  return cache->as_path (ctx);
}

Resolver::CanonicalPath
CanonicalPathRecordImpl::as_path (const NameResolutionContext &ctx)
{
  auto parent_path = get_parent ().as_path (ctx);
  return parent_path.append (
    Resolver::CanonicalPath::inherent_impl_seg (impl_id,
						type_record.as_path (ctx)));
}

Resolver::CanonicalPath
CanonicalPathRecordTraitImpl::as_path (const NameResolutionContext &ctx)
{
  auto parent_path = get_parent ().as_path (ctx);
  return parent_path.append (
    Resolver::CanonicalPath::trait_impl_projection_seg (
      impl_id, trait_path_record.as_path (ctx), type_record.as_path (ctx)));
}

NameResolutionContext::NameResolutionContext ()
  : mappings (Analysis::Mappings::get ()), canonical_ctx (*this)
{}

tl::expected<NodeId, DuplicateNameError>
NameResolutionContext::insert (Identifier name, NodeId id, Namespace ns)
{
  switch (ns)
    {
    case Namespace::Values:
      return values.insert (name, id);
    case Namespace::Types:
      return types.insert (name, id);
    case Namespace::Macros:
      return macros.insert (name, id);
    case Namespace::Labels:
    default:
      // return labels.insert (name, id);
      rust_unreachable ();
    }
}

tl::expected<NodeId, DuplicateNameError>
NameResolutionContext::insert_variant (Identifier name, NodeId id)
{
  return types.insert_variant (name, id);
}

tl::expected<NodeId, DuplicateNameError>
NameResolutionContext::insert_shadowable (Identifier name, NodeId id,
					  Namespace ns)
{
  switch (ns)
    {
    case Namespace::Values:
      return values.insert_shadowable (name, id);
    case Namespace::Types:
      return types.insert_shadowable (name, id);
    case Namespace::Macros:
      return macros.insert_shadowable (name, id);
    case Namespace::Labels:
    default:
      // return labels.insert (name, id);
      rust_unreachable ();
    }
}

tl::expected<NodeId, DuplicateNameError>
NameResolutionContext::insert_globbed (Identifier name, NodeId id, Namespace ns)
{
  switch (ns)
    {
    case Namespace::Values:
      return values.insert_globbed (name, id);
    case Namespace::Types:
      return types.insert_globbed (name, id);
    case Namespace::Macros:
      return macros.insert_globbed (name, id);
    case Namespace::Labels:
    default:
      // return labels.insert (name, id);
      rust_unreachable ();
    }
}

void
NameResolutionContext::map_usage (Usage usage, Definition definition)
{
  auto inserted = resolved_nodes.emplace (usage, definition).second;

  // is that valid?
  rust_assert (inserted);
}

tl::optional<NodeId>
NameResolutionContext::lookup (NodeId usage) const
{
  auto it = resolved_nodes.find (Usage (usage));

  if (it == resolved_nodes.end ())
    return tl::nullopt;

  return it->second.id;
}

void
NameResolutionContext::scoped (Rib::Kind rib_kind, NodeId id,
			       std::function<void (void)> lambda,
			       tl::optional<Identifier> path)
{
  // NOTE: You must be at the root node when pushing the prelude rib.
  values.push (rib_kind, id, path);
  types.push (rib_kind, id, path);
  macros.push (rib_kind, id, path);
  // labels.push (rib, id);

  lambda ();

  values.pop ();
  types.pop ();
  macros.pop ();
  // labels.pop (rib);
}

void
NameResolutionContext::scoped (Rib::Kind rib_kind, Namespace ns,
			       NodeId scope_id,
			       std::function<void (void)> lambda,
			       tl::optional<Identifier> path)
{
  // This could work... I'm not sure why you would want to do this though.
  rust_assert (rib_kind != Rib::Kind::Prelude);

  switch (ns)
    {
    case Namespace::Values:
      values.push (rib_kind, scope_id, path);
      break;
    case Namespace::Types:
      types.push (rib_kind, scope_id, path);
      break;
    case Namespace::Labels:
    case Namespace::Macros:
      gcc_unreachable ();
    }

  lambda ();

  switch (ns)
    {
    case Namespace::Values:
      values.pop ();
      break;
    case Namespace::Types:
      types.pop ();
      break;
    case Namespace::Labels:
    case Namespace::Macros:
      gcc_unreachable ();
    }
}

} // namespace Resolver2_0
} // namespace Rust
