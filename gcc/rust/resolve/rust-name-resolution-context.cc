// Copyright (C) 2020-2026 Free Software Foundation, Inc.

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
#include "rust-rib.h"
#include "rust-system.h"

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
CanonicalPathRecordCrateRoot::as_path (const NameResolutionContext &,
				       Namespace ns)
{
  auto ret = Resolver::CanonicalPath::new_seg (node_id, seg);
  ret.set_crate_num (crate_num);
  return ret;
}

Resolver::CanonicalPath
CanonicalPathRecordNormal::as_path (const NameResolutionContext &ctx,
				    Namespace ns)
{
  auto &parent = ctx.canonical_ctx.get_record (get_parent ());
  auto parent_path = parent.as_path (ctx, ns);
  return parent_path.append (Resolver::CanonicalPath::new_seg (node_id, seg));
}

Resolver::CanonicalPath
CanonicalPathRecordLookup::as_path (const NameResolutionContext &ctx,
				    Namespace ns)
{
  if (!cache)
    {
      // TODO: what namespace do we use here? can the caller give one?
      auto res = ctx.lookup (lookup_id, ns).and_then ([&ctx] (NodeId id) {
	return ctx.canonical_ctx.get_record_opt (id);
      });

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
  return cache->as_path (ctx, ns);
}

Resolver::CanonicalPath
CanonicalPathRecordImpl::as_path (const NameResolutionContext &ctx,
				  Namespace ns)
{
  auto parent_path
    = ctx.canonical_ctx.get_record (get_parent ()).as_path (ctx, ns);
  return parent_path.append (
    Resolver::CanonicalPath::inherent_impl_seg (impl_id,
						type_record.as_path (ctx, ns)));
}

Resolver::CanonicalPath
CanonicalPathRecordTraitImpl::as_path (const NameResolutionContext &ctx,
				       Namespace ns)
{
  // Maybe this doesn't need the namespace and will always be in the types NS?
  auto parent_path
    = ctx.canonical_ctx.get_record (get_parent ()).as_path (ctx, ns);
  return parent_path.append (
    Resolver::CanonicalPath::trait_impl_projection_seg (
      impl_id, trait_path_record.as_path (ctx, ns),
      type_record.as_path (ctx, ns)));
}

NameResolutionContext::NameResolutionContext ()
  : root (std::make_unique<Node> (Rib::Kind::Normal, UNKNOWN_NODEID)),
    lang_prelude (
      std::make_unique<Node> (Rib::Kind::Prelude, UNKNOWN_NODEID, *root)),
    extern_prelude (
      std::make_unique<Node> (Rib::Kind::Prelude, UNKNOWN_NODEID)),
    values (*root, *lang_prelude, *extern_prelude),
    types (*root, *lang_prelude, *extern_prelude),
    macros (*root, *lang_prelude, *extern_prelude),
    labels (*root, *lang_prelude, *extern_prelude),
    mappings (Analysis::Mappings::get ()), canonical_ctx (*this)
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
      return labels.insert (name, id);
      rust_unreachable ();
    }
}

tl::expected<NodeId, DuplicateNameError>
NameResolutionContext::insert_variant (Identifier name, NodeId id,
				       bool is_also_value)
{
  auto res = types.insert_variant (name, id);
  if (res.has_value () && is_also_value)
    res = values.insert_variant (name, id);
  return res;
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
      return labels.insert (name, id);
    default:
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
      return labels.insert (name, id);
    default:
      rust_unreachable ();
    }
}

// TODO: Maybe this should take a NamespacedDefinition as argument?
void
NameResolutionContext::map_usage (Usage usage, Definition definition,
				  Namespace ns)
{
  switch (ns)
    {
    case Namespace::Values:
      values.map_usage (usage, definition);
      break;
    case Namespace::Types:
      types.map_usage (usage, definition);
      break;
    case Namespace::Labels:
      labels.map_usage (usage, definition);
      break;
    case Namespace::Macros:
      macros.map_usage (usage, definition);
      break;
    }
}

tl::optional<NodeId>
NameResolutionContext::lookup (NodeId usage, Namespace ns) const
{
  switch (ns)
    {
    case Namespace::Values:
      return values.lookup (usage);
    case Namespace::Types:
      return types.lookup (usage);
    case Namespace::Labels:
      return labels.lookup (usage);
    case Namespace::Macros:
      return macros.lookup (usage);
    default:
      rust_unreachable ();
    }
}

tl::optional<NameResolutionContext::NSLookup>
NameResolutionContext::lookup (NodeId usage, Namespace ns1, Namespace ns2) const
{
  if (auto result = lookup (usage, ns1))
    return NSLookup (*result, ns1);

  return lookup (usage, ns2).map ([&ns2] (NodeId id) {
    return NSLookup (id, ns2);
  });
}

tl::optional<NameResolutionContext::NSLookup>
NameResolutionContext::lookup (NodeId usage, Namespace ns1, Namespace ns2,
			       Namespace ns3) const
{
  if (auto result = lookup (usage, ns1, ns2))
    return result;

  return lookup (usage, ns3).map ([&ns3] (NodeId id) {
    return NSLookup (id, ns3);
  });
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
  labels.push (rib_kind, id, path);

  lambda ();

  values.pop ();
  types.pop ();
  macros.pop ();
  labels.pop ();
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
      labels.push (rib_kind, scope_id, path);
      break;
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
      labels.pop ();
      break;
    case Namespace::Macros:
      gcc_unreachable ();
    }
}

void
NameResolutionContext::merge (NameResolutionContext &other, NodeId at)
{
  // TODO: merge once, for all namespaces at once

  auto merge_fstack = [&] (auto &stack, auto &other_stack, Namespace ns) {
    auto node = stack.dfs_node (stack.root, at);
    if (node)
      {
	auto &extern_crate_node = node.value ();
	for (auto kv : other_stack.root.children)
	  {
	    auto link = kv.first;
	    auto child = kv.second;
	    extern_crate_node.insert_child (link, child);
	    child.parent = extern_crate_node;
	  }
	for (auto kv : other_stack.root.rib (ns).get_values ())
	  {
	    auto name = kv.first;
	    auto def = kv.second;
	    extern_crate_node.rib (ns).insert (name, def);
	  }
      }
    stack.resolved_nodes.insert (other_stack.resolved_nodes.begin (),
				 other_stack.resolved_nodes.end ());
  };

  merge_fstack (values, other.values, Namespace::Values);
  merge_fstack (types, other.types, Namespace::Types);
  merge_fstack (macros, other.macros, Namespace::Macros);
  merge_fstack (labels, other.labels, Namespace::Labels);
  canonical_ctx.merge (std::move (other.canonical_ctx));
}

#if 0
void
NameResolutionContext::flatten ()
{
  values.flatten ();
  types.flatten ();
  macros.flatten ();
  labels.flatten ();
}
#endif

} // namespace Resolver2_0
} // namespace Rust
