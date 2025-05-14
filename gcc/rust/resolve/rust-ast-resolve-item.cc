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

#include "rust-ast-resolve-item.h"
#include "rust-ast-full-decls.h"
#include "rust-ast-resolve-toplevel.h"
#include "rust-ast-resolve-type.h"
#include "rust-ast-resolve-pattern.h"
#include "rust-ast-resolve-path.h"

#include "rust-item.h"
#include "selftest.h"

namespace Rust {
namespace Resolver {

ResolveTraitItems::ResolveTraitItems (const CanonicalPath &prefix,
				      const CanonicalPath &canonical_prefix)
  : ResolverBase (), prefix (prefix), canonical_prefix (canonical_prefix)
{}

void
ResolveTraitItems::go (AST::AssociatedItem *item, const CanonicalPath &prefix,
		       const CanonicalPath &canonical_prefix)
{
  if (item->is_marked_for_strip ())
    return;

  ResolveTraitItems resolver (prefix, canonical_prefix);
  item->accept_vis (resolver);
}

void
ResolveTraitItems::visit (AST::Function &function)
{
  auto decl
    = CanonicalPath::new_seg (function.get_node_id (),
			      function.get_function_name ().as_string ());
  auto path = prefix.append (decl);
  auto cpath = canonical_prefix.append (decl);
  mappings.insert_canonical_path (function.get_node_id (), cpath);

  NodeId scope_node_id = function.get_node_id ();
  resolver->get_name_scope ().push (scope_node_id);
  resolver->get_type_scope ().push (scope_node_id);
  resolver->get_label_scope ().push (scope_node_id);
  resolver->push_new_name_rib (resolver->get_name_scope ().peek ());
  resolver->push_new_type_rib (resolver->get_type_scope ().peek ());
  resolver->push_new_label_rib (resolver->get_label_scope ().peek ());

  if (function.has_generics ())
    ResolveGenericParams::go (function.get_generic_params (), prefix,
			      canonical_prefix);

  if (function.has_return_type ())
    ResolveType::go (function.get_return_type ());

  // self turns into (self: Self) as a function param
  std::vector<PatternBinding> bindings
    = {PatternBinding (PatternBoundCtx::Product, std::set<Identifier> ())};

  // we make a new scope so the names of parameters are resolved and shadowed
  // correctly
  for (auto &p : function.get_function_params ())
    {
      if (p->is_variadic ())
	{
	  auto param = static_cast<AST::VariadicParam &> (*p);
	  PatternDeclaration::go (param.get_pattern (), Rib::ItemType::Param,
				  bindings);
	}
      else if (p->is_self ())
	{
	  auto &param = static_cast<AST::SelfParam &> (*p);
	  // FIXME: which location should be used for Rust::Identifier `self`?
	  AST::IdentifierPattern self_pattern (
	    param.get_node_id (), {"self"}, param.get_locus (),
	    param.get_has_ref (), param.get_is_mut (),
	    std::unique_ptr<AST::Pattern> (nullptr));

	  PatternDeclaration::go (self_pattern, Rib::ItemType::Param);

	  if (param.has_type ())
	    {
	      // This shouldn't happen the parser should already error for this
	      rust_assert (!param.get_has_ref ());
	      ResolveType::go (param.get_type ());
	    }
	  else
	    {
	      // here we implicitly make self have a type path of Self
	      std::vector<std::unique_ptr<AST::TypePathSegment>> segments;
	      segments.push_back (std::unique_ptr<AST::TypePathSegment> (
		new AST::TypePathSegment ("Self", false, param.get_locus ())));

	      AST::TypePath self_type_path (std::move (segments),
					    param.get_locus ());
	      ResolveType::go (self_type_path);
	    }
	}
      else
	{
	  auto &param = static_cast<AST::FunctionParam &> (*p);
	  ResolveType::go (param.get_type ());
	  PatternDeclaration::go (param.get_pattern (), Rib::ItemType::Param,
				  bindings);
	}
    }

  if (function.has_where_clause ())
    ResolveWhereClause::Resolve (function.get_where_clause ());

  // trait items have an optional body
  if (function.has_body ())
    ResolveExpr::go (*function.get_definition ().value (), path, cpath);

  resolver->get_name_scope ().pop ();
  resolver->get_type_scope ().pop ();
  resolver->get_label_scope ().pop ();
}
void
ResolveTraitItems::visit (AST::TraitItemType &type)
{
  auto decl = CanonicalPath::new_seg (type.get_node_id (),
				      type.get_identifier ().as_string ());
  auto path = prefix.append (decl);
  auto cpath = canonical_prefix.append (decl);
  mappings.insert_canonical_path (type.get_node_id (), cpath);

  for (auto &bound : type.get_type_param_bounds ())
    ResolveTypeBound::go (*bound);
}

void
ResolveTraitItems::visit (AST::TraitItemConst &constant)
{
  auto decl = CanonicalPath::new_seg (constant.get_node_id (),
				      constant.get_identifier ().as_string ());
  auto path = prefix.append (decl);
  auto cpath = canonical_prefix.append (decl);
  mappings.insert_canonical_path (constant.get_node_id (), cpath);

  ResolveType::go (constant.get_type ());

  if (constant.has_expr ())
    ResolveExpr::go (constant.get_expr (), path, cpath);
}

ResolveItem::ResolveItem (const CanonicalPath &prefix,
			  const CanonicalPath &canonical_prefix)
  : ResolverBase (), prefix (prefix), canonical_prefix (canonical_prefix)
{}

void
ResolveItem::go (AST::Item &item, const CanonicalPath &prefix,
		 const CanonicalPath &canonical_prefix)
{
  ResolveItem resolver (prefix, canonical_prefix);
  item.accept_vis (resolver);
}

void
ResolveItem::visit (AST::TypeAlias &alias)
{
  auto talias
    = CanonicalPath::new_seg (alias.get_node_id (),
			      alias.get_new_type_name ().as_string ());
  auto path = prefix.append (talias);
  auto cpath = canonical_prefix.append (talias);
  mappings.insert_canonical_path (alias.get_node_id (), cpath);

  NodeId scope_node_id = alias.get_node_id ();
  resolver->get_type_scope ().push (scope_node_id);

  if (alias.has_generics ())
    ResolveGenericParams::go (alias.get_generic_params (), prefix,
			      canonical_prefix);

  if (alias.has_where_clause ())
    ResolveWhereClause::Resolve (alias.get_where_clause ());

  ResolveType::go (alias.get_type_aliased ());

  resolver->get_type_scope ().pop ();
}

void
ResolveItem::visit (AST::Module &module)
{
  auto mod = CanonicalPath::new_seg (module.get_node_id (),
				     module.get_name ().as_string ());
  auto path = prefix.append (mod);
  auto cpath = canonical_prefix.append (mod);
  mappings.insert_canonical_path (module.get_node_id (), cpath);

  resolve_visibility (module.get_visibility ());

  NodeId scope_node_id = module.get_node_id ();
  resolver->get_name_scope ().push (scope_node_id);
  resolver->get_type_scope ().push (scope_node_id);
  resolver->get_label_scope ().push (scope_node_id);
  resolver->push_new_name_rib (resolver->get_name_scope ().peek ());
  resolver->push_new_type_rib (resolver->get_type_scope ().peek ());
  resolver->push_new_label_rib (resolver->get_label_scope ().peek ());

  // FIXME: Should we reinsert a child here? Any reason we ResolveTopLevel::go
  // in ResolveTopLevel::visit (AST::Module) as well as here?
  for (auto &item : module.get_items ())
    ResolveTopLevel::go (*item, CanonicalPath::create_empty (), cpath);

  resolver->push_new_module_scope (module.get_node_id ());
  for (auto &item : module.get_items ())
    ResolveItem::go (*item, path, cpath);

  resolver->pop_module_scope ();

  resolver->get_name_scope ().pop ();
  resolver->get_type_scope ().pop ();
  resolver->get_label_scope ().pop ();
}

void
ResolveItem::visit (AST::TupleStruct &struct_decl)
{
  auto decl
    = CanonicalPath::new_seg (struct_decl.get_node_id (),
			      struct_decl.get_identifier ().as_string ());
  auto path = prefix.append (decl);
  auto cpath = canonical_prefix.append (decl);
  mappings.insert_canonical_path (struct_decl.get_node_id (), cpath);

  resolve_visibility (struct_decl.get_visibility ());

  NodeId scope_node_id = struct_decl.get_node_id ();
  resolver->get_type_scope ().push (scope_node_id);

  if (struct_decl.has_generics ())
    ResolveGenericParams::go (struct_decl.get_generic_params (), prefix,
			      canonical_prefix);

  if (struct_decl.has_where_clause ())
    ResolveWhereClause::Resolve (struct_decl.get_where_clause ());

  for (AST::TupleField &field : struct_decl.get_fields ())
    {
      if (field.get_field_type ().is_marked_for_strip ())
	continue;

      resolve_visibility (field.get_visibility ());

      ResolveType::go (field.get_field_type ());
    }

  resolver->get_type_scope ().pop ();
}

void
ResolveItem::visit (AST::Enum &enum_decl)
{
  auto decl = CanonicalPath::new_seg (enum_decl.get_node_id (),
				      enum_decl.get_identifier ().as_string ());
  auto path = prefix.append (decl);
  auto cpath = canonical_prefix.append (decl);
  mappings.insert_canonical_path (enum_decl.get_node_id (), cpath);

  resolve_visibility (enum_decl.get_visibility ());

  NodeId scope_node_id = enum_decl.get_node_id ();
  resolver->get_type_scope ().push (scope_node_id);

  if (enum_decl.has_generics ())
    ResolveGenericParams::go (enum_decl.get_generic_params (), prefix,
			      canonical_prefix);

  if (enum_decl.has_where_clause ())
    ResolveWhereClause::Resolve (enum_decl.get_where_clause ());

  /* The actual fields are inside the variants.  */
  for (auto &variant : enum_decl.get_variants ())
    ResolveItem::go (*variant, path, cpath);

  resolver->get_type_scope ().pop ();
}

/* EnumItem doesn't need to be handled, no fields.  */
void
ResolveItem::visit (AST::EnumItem &item)
{
  // Since at this point we cannot have visibilities on enum items anymore, we
  // can skip handling them

  auto decl = CanonicalPath::new_seg (item.get_node_id (),
				      item.get_identifier ().as_string ());
  auto path = prefix.append (decl);
  auto cpath = canonical_prefix.append (decl);
  mappings.insert_canonical_path (item.get_node_id (), cpath);
}

void
ResolveItem::visit (AST::EnumItemTuple &item)
{
  auto decl = CanonicalPath::new_seg (item.get_node_id (),
				      item.get_identifier ().as_string ());
  auto path = prefix.append (decl);
  auto cpath = canonical_prefix.append (decl);
  mappings.insert_canonical_path (item.get_node_id (), cpath);

  for (auto &field : item.get_tuple_fields ())
    {
      if (field.get_field_type ().is_marked_for_strip ())
	continue;

      ResolveType::go (field.get_field_type ());
    }
}

void
ResolveItem::visit (AST::EnumItemStruct &item)
{
  auto decl = CanonicalPath::new_seg (item.get_node_id (),
				      item.get_identifier ().as_string ());
  auto path = prefix.append (decl);
  auto cpath = canonical_prefix.append (decl);
  mappings.insert_canonical_path (item.get_node_id (), cpath);

  for (auto &field : item.get_struct_fields ())
    {
      if (field.get_field_type ().is_marked_for_strip ())
	continue;

      ResolveType::go (field.get_field_type ());
    }
}

void
ResolveItem::visit (AST::EnumItemDiscriminant &item)
{
  auto decl = CanonicalPath::new_seg (item.get_node_id (),
				      item.get_identifier ().as_string ());
  auto path = prefix.append (decl);
  auto cpath = canonical_prefix.append (decl);

  mappings.insert_canonical_path (item.get_node_id (), cpath);

  ResolveExpr::go (item.get_expr (), path, cpath);
}

void
ResolveItem::visit (AST::StructStruct &struct_decl)
{
  auto decl
    = CanonicalPath::new_seg (struct_decl.get_node_id (),
			      struct_decl.get_identifier ().as_string ());
  auto path = prefix.append (decl);
  auto cpath = canonical_prefix.append (decl);
  mappings.insert_canonical_path (struct_decl.get_node_id (), cpath);

  resolve_visibility (struct_decl.get_visibility ());

  NodeId scope_node_id = struct_decl.get_node_id ();
  resolver->get_type_scope ().push (scope_node_id);

  if (struct_decl.has_generics ())
    ResolveGenericParams::go (struct_decl.get_generic_params (), prefix,
			      canonical_prefix);

  if (struct_decl.has_where_clause ())
    ResolveWhereClause::Resolve (struct_decl.get_where_clause ());

  for (AST::StructField &field : struct_decl.get_fields ())
    {
      if (field.get_field_type ().is_marked_for_strip ())
	continue;

      resolve_visibility (field.get_visibility ());

      ResolveType::go (field.get_field_type ());
    }

  resolver->get_type_scope ().pop ();
}

void
ResolveItem::visit (AST::Union &union_decl)
{
  auto decl
    = CanonicalPath::new_seg (union_decl.get_node_id (),
			      union_decl.get_identifier ().as_string ());
  auto path = prefix.append (decl);
  auto cpath = canonical_prefix.append (decl);
  mappings.insert_canonical_path (union_decl.get_node_id (), cpath);

  resolve_visibility (union_decl.get_visibility ());

  NodeId scope_node_id = union_decl.get_node_id ();
  resolver->get_type_scope ().push (scope_node_id);

  if (union_decl.has_generics ())
    ResolveGenericParams::go (union_decl.get_generic_params (), prefix,
			      canonical_prefix);

  if (union_decl.has_where_clause ())
    ResolveWhereClause::Resolve (union_decl.get_where_clause ());

  for (AST::StructField &field : union_decl.get_variants ())
    {
      if (field.get_field_type ().is_marked_for_strip ())
	continue;

      ResolveType::go (field.get_field_type ());
    }

  resolver->get_type_scope ().pop ();
}

void
ResolveItem::visit (AST::StaticItem &var)
{
  auto decl = CanonicalPath::new_seg (var.get_node_id (),
				      var.get_identifier ().as_string ());
  auto path = prefix.append (decl);
  auto cpath = canonical_prefix.append (decl);
  mappings.insert_canonical_path (var.get_node_id (), cpath);

  ResolveType::go (var.get_type ());
  ResolveExpr::go (var.get_expr (), path, cpath);
}

void
ResolveItem::visit (AST::ConstantItem &constant)
{
  auto decl = CanonicalPath::new_seg (constant.get_node_id (),
				      constant.get_identifier ());
  auto path = prefix.append (decl);
  auto cpath = canonical_prefix.append (decl);
  mappings.insert_canonical_path (constant.get_node_id (), cpath);

  resolve_visibility (constant.get_visibility ());

  ResolveType::go (constant.get_type ());
  ResolveExpr::go (constant.get_expr (), path, cpath);
}

void
ResolveItem::visit (AST::Function &function)
{
  auto decl
    = CanonicalPath::new_seg (function.get_node_id (),
			      function.get_function_name ().as_string ());
  auto path = prefix.append (decl);
  auto cpath = canonical_prefix.append (decl);

  mappings.insert_canonical_path (function.get_node_id (), cpath);

  resolve_visibility (function.get_visibility ());

  NodeId scope_node_id = function.get_node_id ();
  resolver->get_name_scope ().push (scope_node_id);
  resolver->get_type_scope ().push (scope_node_id);
  resolver->get_label_scope ().push (scope_node_id);
  resolver->push_new_name_rib (resolver->get_name_scope ().peek ());
  resolver->push_new_type_rib (resolver->get_type_scope ().peek ());
  resolver->push_new_label_rib (resolver->get_label_scope ().peek ());

  if (function.has_generics ())
    ResolveGenericParams::go (function.get_generic_params (), prefix,
			      canonical_prefix);

  // resolve any where clause items
  if (function.has_where_clause ())
    ResolveWhereClause::Resolve (function.get_where_clause ());

  if (function.has_return_type ())
    ResolveType::go (function.get_return_type ());

  if (function.has_self_param ())
    {
      // self turns into (self: Self) as a function param
      AST::Param &s_param = function.get_self_param ();
      auto &self_param = static_cast<AST::SelfParam &> (s_param);

      // FIXME: which location should be used for Rust::Identifier `self`?
      AST::IdentifierPattern self_pattern (
	self_param.get_node_id (), {"self"}, self_param.get_locus (),
	self_param.get_has_ref (), self_param.get_is_mut (),
	std::unique_ptr<AST::Pattern> (nullptr));
      PatternDeclaration::go (self_pattern, Rib::ItemType::Param);

      if (self_param.has_type ())
	{
	  // This shouldn't happen the parser should already error for this
	  rust_assert (!self_param.get_has_ref ());
	  ResolveType::go (self_param.get_type ());
	}
      else
	{
	  // here we implicitly make self have a type path of Self
	  std::vector<std::unique_ptr<AST::TypePathSegment>> segments;
	  segments.push_back (std::unique_ptr<AST::TypePathSegment> (
	    new AST::TypePathSegment ("Self", false, self_param.get_locus ())));

	  AST::TypePath self_type_path (std::move (segments),
					self_param.get_locus ());
	  ResolveType::go (self_type_path);
	}
    }

  std::vector<PatternBinding> bindings
    = {PatternBinding (PatternBoundCtx::Product, std::set<Identifier> ())};

  // we make a new scope so the names of parameters are resolved and shadowed
  // correctly
  for (auto &p : function.get_function_params ())
    {
      if (p->is_variadic ())
	{
	  auto &param = static_cast<AST::VariadicParam &> (*p);
	  if (param.has_pattern ())
	    PatternDeclaration::go (param.get_pattern (), Rib::ItemType::Param,
				    bindings);
	}
      else if (p->is_self ())
	{
	  auto &param = static_cast<AST::SelfParam &> (*p);
	  if (param.has_type ())
	    ResolveType::go (param.get_type ());
	}
      else
	{
	  auto &param = static_cast<AST::FunctionParam &> (*p);
	  ResolveType::go (param.get_type ());
	  PatternDeclaration::go (param.get_pattern (), Rib::ItemType::Param,
				  bindings);
	}
    }

  // resolve the function body
  ResolveExpr::go (*function.get_definition ().value (), path, cpath);

  resolver->get_name_scope ().pop ();
  resolver->get_type_scope ().pop ();
  resolver->get_label_scope ().pop ();
}

void
ResolveItem::visit (AST::InherentImpl &impl_block)
{
  NodeId scope_node_id = impl_block.get_node_id ();
  resolver->get_name_scope ().push (scope_node_id);
  resolver->get_type_scope ().push (scope_node_id);
  resolver->push_new_name_rib (resolver->get_name_scope ().peek ());
  resolver->push_new_type_rib (resolver->get_type_scope ().peek ());

  resolve_visibility (impl_block.get_visibility ());

  if (impl_block.has_generics ())
    ResolveGenericParams::go (impl_block.get_generic_params (), prefix,
			      canonical_prefix);

  // resolve any where clause items
  if (impl_block.has_where_clause ())
    ResolveWhereClause::Resolve (impl_block.get_where_clause ());

  // FIXME this needs to be protected behind nominal type-checks see:
  // rustc --explain E0118
  // issue #2634
  ResolveType::go (impl_block.get_type ());

  // Setup paths
  CanonicalPath self_cpath = CanonicalPath::create_empty ();
  bool ok = ResolveTypeToCanonicalPath::go (impl_block.get_type (), self_cpath);
  if (!ok)
    {
      resolver->get_name_scope ().pop ();
      resolver->get_type_scope ().pop ();
      resolver->get_label_scope ().pop ();
      return;
    }

  rust_debug ("AST::InherentImpl resolve Self: {%s}",
	      self_cpath.get ().c_str ());

  CanonicalPath impl_type = self_cpath;
  CanonicalPath impl_type_seg
    = CanonicalPath::inherent_impl_seg (impl_block.get_node_id (), impl_type);
  CanonicalPath impl_prefix = prefix.append (impl_type_seg);

  // see https://godbolt.org/z/a3vMbsT6W
  CanonicalPath cpath = CanonicalPath::create_empty ();
  if (canonical_prefix.size () <= 1)
    {
      cpath = impl_prefix;
    }
  else
    {
      std::string seg_buf = "<impl " + self_cpath.get () + ">";
      CanonicalPath seg
	= CanonicalPath::new_seg (impl_block.get_node_id (), seg_buf);
      cpath = canonical_prefix.append (seg);
    }

  // done setup paths

  auto Self
    = CanonicalPath::get_big_self (impl_block.get_type ().get_node_id ());

  resolver->get_type_scope ().insert (Self,
				      impl_block.get_type ().get_node_id (),
				      impl_block.get_type ().get_locus ());

  for (auto &impl_item : impl_block.get_impl_items ())
    {
      rust_debug (
	"AST::InherentImpl resolve_impl_item: impl_prefix={%s} cpath={%s}",
	impl_prefix.get ().c_str (), cpath.get ().c_str ());
      resolve_impl_item (*impl_item, impl_prefix, cpath);
    }

  resolver->get_type_scope ().peek ()->clear_name (
    Self, impl_block.get_type ().get_node_id ());

  resolver->get_type_scope ().pop ();
  resolver->get_name_scope ().pop ();
}

void
ResolveItem::visit (AST::TraitImpl &impl_block)
{
  NodeId scope_node_id = impl_block.get_node_id ();

  resolve_visibility (impl_block.get_visibility ());

  resolver->get_name_scope ().push (scope_node_id);
  resolver->get_type_scope ().push (scope_node_id);
  resolver->get_label_scope ().push (scope_node_id);
  resolver->push_new_name_rib (resolver->get_name_scope ().peek ());
  resolver->push_new_type_rib (resolver->get_type_scope ().peek ());
  resolver->push_new_label_rib (resolver->get_label_scope ().peek ());

  if (impl_block.has_generics ())
    ResolveGenericParams::go (impl_block.get_generic_params (), prefix,
			      canonical_prefix);

  // resolve any where clause items
  if (impl_block.has_where_clause ())
    ResolveWhereClause::Resolve (impl_block.get_where_clause ());

  // CanonicalPath canonical_trait_type = CanonicalPath::create_empty ();
  NodeId trait_resolved_node = ResolveType::go (impl_block.get_trait_path ());
  if (trait_resolved_node == UNKNOWN_NODEID)
    {
      resolver->get_name_scope ().pop ();
      resolver->get_type_scope ().pop ();
      resolver->get_label_scope ().pop ();
      return;
    }

  //   CanonicalPath canonical_impl_type = CanonicalPath::create_empty ();
  NodeId type_resolved_node = ResolveType::go (impl_block.get_type ());
  if (type_resolved_node == UNKNOWN_NODEID)
    {
      resolver->get_name_scope ().pop ();
      resolver->get_type_scope ().pop ();
      resolver->get_label_scope ().pop ();
      return;
    }

  bool ok = true;

  // setup paths
  CanonicalPath canonical_trait_type = CanonicalPath::create_empty ();

  ok = ResolveTypeToCanonicalPath::go (impl_block.get_trait_path (),
				       canonical_trait_type);
  if (!ok)
    {
      resolver->get_name_scope ().pop ();
      resolver->get_type_scope ().pop ();
      resolver->get_label_scope ().pop ();
      return;
    }

  rust_debug ("AST::TraitImpl resolve trait type: {%s}",
	      canonical_trait_type.get ().c_str ());

  CanonicalPath canonical_impl_type = CanonicalPath::create_empty ();
  ok = ResolveTypeToCanonicalPath::go (impl_block.get_type (),
				       canonical_impl_type);
  if (!ok)
    {
      resolver->get_name_scope ().pop ();
      resolver->get_type_scope ().pop ();
      resolver->get_label_scope ().pop ();
      return;
    }

  rust_debug ("AST::TraitImpl resolve self: {%s}",
	      canonical_impl_type.get ().c_str ());

  // raw paths
  CanonicalPath impl_type_seg = canonical_impl_type;
  CanonicalPath trait_type_seg = canonical_trait_type;
  CanonicalPath projection
    = CanonicalPath::trait_impl_projection_seg (impl_block.get_node_id (),
						trait_type_seg, impl_type_seg);
  CanonicalPath impl_prefix = prefix.append (projection);

  // setup canonical-path
  CanonicalPath canonical_projection
    = CanonicalPath::trait_impl_projection_seg (impl_block.get_node_id (),
						canonical_trait_type,
						canonical_impl_type);
  CanonicalPath cpath = CanonicalPath::create_empty ();
  if (canonical_prefix.size () <= 1)
    {
      cpath = canonical_projection;
    }
  else
    {
      std::string projection_str = canonical_projection.get ();
      std::string seg_buf
	= "<impl " + projection_str.substr (1, projection_str.size () - 2)
	  + ">";
      CanonicalPath seg
	= CanonicalPath::new_seg (impl_block.get_node_id (), seg_buf);
      cpath = canonical_prefix.append (seg);
    }

  // DONE setup canonical-path

  auto Self
    = CanonicalPath::get_big_self (impl_block.get_type ().get_node_id ());

  resolver->get_type_scope ().insert (Self,
				      impl_block.get_type ().get_node_id (),
				      impl_block.get_type ().get_locus ());

  for (auto &impl_item : impl_block.get_impl_items ())
    {
      rust_debug (
	"AST::TraitImpl resolve_impl_item: impl_prefix={%s} cpath={%s}",
	impl_prefix.get ().c_str (), cpath.get ().c_str ());
      resolve_impl_item (*impl_item, impl_prefix, cpath);
    }

  Rib *r = resolver->get_type_scope ().peek ();
  r->clear_name (Self, impl_block.get_type ().get_node_id ());

  resolver->get_name_scope ().pop ();
  resolver->get_type_scope ().pop ();
  resolver->get_label_scope ().pop ();
}

void
ResolveItem::visit (AST::Trait &trait)
{
  NodeId scope_node_id = trait.get_node_id ();

  resolve_visibility (trait.get_visibility ());

  resolver->get_name_scope ().push (scope_node_id);
  resolver->get_type_scope ().push (scope_node_id);
  resolver->push_new_name_rib (resolver->get_name_scope ().peek ());
  resolver->push_new_type_rib (resolver->get_type_scope ().peek ());

  ResolveGenericParams::go_single (trait.get_implicit_self (), prefix,
				   canonical_prefix);
  ResolveGenericParams::go (trait.get_generic_params (), prefix,
			    canonical_prefix);

  // Self is an implicit TypeParam so lets mark it as such
  resolver->get_type_scope ().append_reference_for_def (
    trait.get_node_id (), trait.get_implicit_self ().get_node_id ());

  if (trait.has_type_param_bounds ())
    {
      for (auto &bound : trait.get_type_param_bounds ())
	{
	  ResolveTypeBound::go (*bound);
	}
    }

  // resolve any where clause items
  if (trait.has_where_clause ())
    ResolveWhereClause::Resolve (trait.get_where_clause ());

  // resolve the paths
  CanonicalPath path = CanonicalPath::create_empty ();
  CanonicalPath cpath = CanonicalPath::create_empty ();
  //

  for (auto &item : trait.get_trait_items ())
    {
      ResolveTraitItems::go (item.get (), path, cpath);
    }

  resolver->get_type_scope ().pop ();
  resolver->get_name_scope ().pop ();
}

void
ResolveItem::visit (AST::ExternBlock &extern_block)
{
  resolve_visibility (extern_block.get_visibility ());

  for (auto &item : extern_block.get_extern_items ())
    {
      resolve_extern_item (*item);
    }
}

void
ResolveItem::resolve_impl_item (AST::AssociatedItem &item,
				const CanonicalPath &prefix,
				const CanonicalPath &canonical_prefix)
{
  ResolveImplItems::go (item, prefix, canonical_prefix);
}

void
ResolveItem::resolve_extern_item (AST::ExternalItem &item)
{
  ResolveExternItem::go (item, prefix, canonical_prefix);
}

static void
flatten_glob (const AST::UseTreeGlob &glob, std::vector<Import> &imports);
static void
flatten_rebind (const AST::UseTreeRebind &glob, std::vector<Import> &imports);
static void
flatten_list (const AST::UseTreeList &glob, std::vector<Import> &imports);

static void
flatten (const AST::UseTree *tree, std::vector<Import> &imports)
{
  switch (tree->get_kind ())
    {
      case AST::UseTree::Glob: {
	auto glob = static_cast<const AST::UseTreeGlob *> (tree);
	flatten_glob (*glob, imports);
	break;
      }
      case AST::UseTree::Rebind: {
	auto rebind = static_cast<const AST::UseTreeRebind *> (tree);
	flatten_rebind (*rebind, imports);
	break;
      }
      case AST::UseTree::List: {
	auto list = static_cast<const AST::UseTreeList *> (tree);
	flatten_list (*list, imports);
	break;
      }
      break;
    }
}

static void
flatten_glob (const AST::UseTreeGlob &glob, std::vector<Import> &imports)
{
  if (glob.has_path ())
    imports.emplace_back (glob.get_path (), true, std::string ());
}

static void
flatten_rebind (const AST::UseTreeRebind &rebind, std::vector<Import> &imports)
{
  auto path = rebind.get_path ();

  std::string label;
  if (rebind.has_identifier ())
    label = rebind.get_identifier ().as_string ();
  else
    label = path.get_final_segment ().as_string ();

  imports.emplace_back (path, false, label);
}

static void
flatten_list (const AST::UseTreeList &list, std::vector<Import> &imports)
{
  auto prefix = AST::SimplePath::create_empty ();
  if (list.has_path ())
    prefix = list.get_path ();

  for (const auto &tree : list.get_trees ())
    {
      // append imports to the main list, then modify them in-place
      auto start_idx = imports.size ();
      flatten (tree.get (), imports);

      for (auto import = imports.begin () + start_idx; import != imports.end ();
	   import++)
	{
	  // avoid duplicate node ids
	  auto prefix_copy
	    = AST::SimplePath ({}, prefix.has_opening_scope_resolution (),
			       prefix.get_locus ());
	  for (auto &seg : prefix.get_segments ())
	    prefix_copy.get_segments ().push_back (
	      AST::SimplePathSegment (seg.get_segment_name (),
				      seg.get_locus ()));

	  import->add_prefix (std::move (prefix_copy));
	}
    }
}

void
Import::add_prefix (AST::SimplePath prefix)
{
  AST::SimplePath old_path (std::move (path));
  path = std::move (prefix);
  std::move (old_path.get_segments ().begin (), old_path.get_segments ().end (),
	     std::back_inserter (path.get_segments ()));
}

/**
 * Flatten a UseDeclaration's UseTree into multiple simple paths to resolve.
 *
 * Given the following use declarations:
 * ```
 * use some::path::to_resolve; #1
 * use some::path::to_glob::*; #2
 * use some::path::{one, two}; #2
 * ```
 *
 * In the first case, we simply want to return a vector with a single
 * SimplePath:
 * [some::path::to_resolve]
 *
 * In the second case, we want to resolve the glob's "origin path":
 * [some::path::to_glob]
 *
 * Finally in the third case, we want to create two SimplePaths to resolve:
 * [some::path::one, some::path::two]
 */
static std::vector<Import>
flatten_use_dec_to_imports (const AST::UseDeclaration &use_item)
{
  auto imports = std::vector<Import> ();

  const auto &tree = use_item.get_tree ();
  flatten (tree.get (), imports);

  return imports;
}

void
ResolveItem::visit (AST::UseDeclaration &use_item)
{
  std::vector<Import> to_resolve = flatten_use_dec_to_imports (use_item);

  // FIXME: I think this does not actually resolve glob use-decls and is going
  // the wrong way about it. RFC #1560 specifies the following:
  //
  // > When we find a glob import, we have to record a 'back link', so that when
  // a public name is added for the supplying module, we can add it for the
  // importing module.
  //
  // Which is the opposite of what we're doing if I understand correctly?

  NodeId current_module = resolver->peek_current_module_scope ();
  for (auto &import : to_resolve)
    {
      auto &path = import.get_path ();

      rust_debug ("resolving use-decl path: [%s]", path.as_string ().c_str ());
      NodeId resolved_node_id = ResolvePath::go (path);
      bool ok = resolved_node_id != UNKNOWN_NODEID;
      if (!ok)
	continue;

      if (import.is_glob ())
	continue;

      auto decl = CanonicalPath::new_seg (resolved_node_id, import.get_name ());
      mappings.insert_module_child_item (current_module, decl);

      resolver->get_type_scope ().insert (decl, resolved_node_id,
					  path.get_locus (),
					  Rib::ItemType::Type);
      rust_debug ("use-decl rexporting: [%s]", decl.get ().c_str ());
    }
}

ResolveImplItems::ResolveImplItems (const CanonicalPath &prefix,
				    const CanonicalPath &canonical_prefix)
  : ResolveItem (prefix, canonical_prefix)
{}

void
ResolveImplItems::go (AST::AssociatedItem &item, const CanonicalPath &prefix,
		      const CanonicalPath &canonical_prefix)
{
  if (item.is_marked_for_strip ())
    return;

  ResolveImplItems resolver (prefix, canonical_prefix);
  item.accept_vis (resolver);
}

void
ResolveImplItems::visit (AST::TypeAlias &alias)
{
  ResolveItem::visit (alias);

  resolve_visibility (alias.get_visibility ());

  // FIXME this stops the erronious unused decls which will be fixed later on
  resolver->get_type_scope ().append_reference_for_def (alias.get_node_id (),
							alias.get_node_id ());
}

void
ResolveExternItem::go (AST::ExternalItem &item, const CanonicalPath &prefix,
		       const CanonicalPath &canonical_prefix)
{
  ResolveExternItem resolver (prefix, canonical_prefix);
  item.accept_vis (resolver);
}

void
ResolveExternItem::visit (AST::Function &function)
{
  NodeId scope_node_id = function.get_node_id ();
  auto decl
    = CanonicalPath::new_seg (function.get_node_id (),
			      function.get_function_name ().as_string ());
  auto path = prefix.append (decl);
  auto cpath = canonical_prefix.append (decl);

  mappings.insert_canonical_path (function.get_node_id (), cpath);

  resolve_visibility (function.get_visibility ());

  resolver->get_name_scope ().push (scope_node_id);
  resolver->get_type_scope ().push (scope_node_id);
  resolver->get_label_scope ().push (scope_node_id);
  resolver->push_new_name_rib (resolver->get_name_scope ().peek ());
  resolver->push_new_type_rib (resolver->get_type_scope ().peek ());
  resolver->push_new_label_rib (resolver->get_label_scope ().peek ());

  // resolve the generics
  if (function.has_generics ())
    ResolveGenericParams::go (function.get_generic_params (), prefix,
			      canonical_prefix);

  if (function.has_return_type ())
    ResolveType::go (function.get_return_type ());

  // we make a new scope so the names of parameters are resolved and shadowed
  // correctly
  for (auto &param : function.get_function_params ())
    if (!param->is_variadic ())
      {
	auto &p = static_cast<AST::FunctionParam &> (*param);
	ResolveType::go (p.get_type ());
      }

  // done
  resolver->get_name_scope ().pop ();
  resolver->get_type_scope ().pop ();
  resolver->get_label_scope ().pop ();
}

void
ResolveExternItem::visit (AST::ExternalStaticItem &item)
{
  resolve_visibility (item.get_visibility ());

  ResolveType::go (item.get_type ());
}

} // namespace Resolver
} // namespace Rust

#if CHECKING_P

namespace selftest {

static void
rust_flatten_nested_glob (void)
{
  auto foo = Rust::AST::SimplePathSegment ("foo", UNDEF_LOCATION);
  auto bar = Rust::AST::SimplePathSegment ("bar", UNDEF_LOCATION);
  auto foobar = Rust::AST::SimplePath ({foo, bar});

  auto glob
    = Rust::AST::UseTreeGlob (Rust::AST::UseTreeGlob::PathType::PATH_PREFIXED,
			      foobar, UNDEF_LOCATION);

  auto imports = std::vector<Rust::Resolver::Import> ();
  Rust::Resolver::flatten_glob (glob, imports);

  ASSERT_TRUE (!imports.empty ());
  ASSERT_EQ (imports.size (), 1);
  ASSERT_EQ (imports[0].get_path ().get_segments ()[0].as_string (), "foo");
  ASSERT_EQ (imports[0].get_path ().get_segments ()[1].as_string (), "bar");
}

static void
rust_flatten_glob (void)
{
  auto frob = Rust::AST::SimplePath::from_str ("frobulator", UNDEF_LOCATION);

  auto glob
    = Rust::AST::UseTreeGlob (Rust::AST::UseTreeGlob::PathType::PATH_PREFIXED,
			      frob, UNDEF_LOCATION);

  auto imports = std::vector<Rust::Resolver::Import> ();
  Rust::Resolver::flatten_glob (glob, imports);

  ASSERT_TRUE (!imports.empty ());
  ASSERT_EQ (imports.size (), 1);
  ASSERT_EQ (imports[0].get_path (), "frobulator");
}

static void
rust_flatten_rebind_none (void)
{
  auto foo = Rust::AST::SimplePathSegment ("foo", UNDEF_LOCATION);
  auto bar = Rust::AST::SimplePathSegment ("bar", UNDEF_LOCATION);
  auto foobar = Rust::AST::SimplePath ({foo, bar});

  auto rebind = Rust::AST::UseTreeRebind (Rust::AST::UseTreeRebind::NONE,
					  foobar, UNDEF_LOCATION);

  auto imports = std::vector<Rust::Resolver::Import> ();
  Rust::Resolver::flatten_rebind (rebind, imports);

  ASSERT_TRUE (!imports.empty ());
  ASSERT_EQ (imports.size (), 1);
  ASSERT_EQ (imports[0].get_path ().get_segments ()[0].as_string (), "foo");
  ASSERT_EQ (imports[0].get_path ().get_segments ()[1].as_string (), "bar");
}

static void
rust_flatten_rebind (void)
{
  auto frob = Rust::AST::SimplePath::from_str ("frobulator", UNDEF_LOCATION);

  auto rebind = Rust::AST::UseTreeRebind (Rust::AST::UseTreeRebind::IDENTIFIER,
					  frob, UNDEF_LOCATION, {"saindoux"});

  auto imports = std::vector<Rust::Resolver::Import> ();
  Rust::Resolver::flatten_rebind (rebind, imports);

  ASSERT_TRUE (!imports.empty ());
  ASSERT_EQ (imports.size (), 1);
  ASSERT_EQ (imports[0].get_path (), "frobulator");
  ASSERT_EQ (imports[0].get_name (), "saindoux");
}

static void
rust_flatten_rebind_nested (void)
{
  auto foo = Rust::AST::SimplePathSegment ("foo", UNDEF_LOCATION);
  auto bar = Rust::AST::SimplePathSegment ("bar", UNDEF_LOCATION);
  auto baz = Rust::AST::SimplePathSegment ("baz", UNDEF_LOCATION);

  auto foo_bar_baz = Rust::AST::SimplePath ({foo, bar, baz});

  auto rebind
    = Rust::AST::UseTreeRebind (Rust::AST::UseTreeRebind::IDENTIFIER,
				foo_bar_baz, UNDEF_LOCATION, {"saindoux"});

  auto imports = std::vector<Rust::Resolver::Import> ();
  Rust::Resolver::flatten_rebind (rebind, imports);

  ASSERT_TRUE (!imports.empty ());
  ASSERT_EQ (imports.size (), 1);
  ASSERT_EQ (imports[0].get_path ().get_segments ()[0].as_string (), "foo");
  ASSERT_EQ (imports[0].get_path ().get_segments ()[1].as_string (), "bar");
  ASSERT_EQ (imports[0].get_path ().get_segments ()[2].as_string (), "baz");
  ASSERT_EQ (imports[0].get_name (), "saindoux");
}

static void
rust_flatten_list (void)
{
  auto foo = Rust::AST::SimplePathSegment ("foo", UNDEF_LOCATION);
  auto bar = Rust::AST::SimplePathSegment ("bar", UNDEF_LOCATION);
  auto foo_bar = Rust::AST::SimplePath ({foo, bar});

  auto baz = Rust::AST::SimplePath::from_str ("baz", UNDEF_LOCATION);
  auto bul = Rust::AST::SimplePath::from_str ("bul", UNDEF_LOCATION);

  // use foo::bar::{baz, bul};

  auto use0 = std::unique_ptr<Rust::AST::UseTree> (
    new Rust::AST::UseTreeRebind (Rust::AST::UseTreeRebind::NONE, baz,
				  UNDEF_LOCATION));
  auto use1 = std::unique_ptr<Rust::AST::UseTree> (
    new Rust::AST::UseTreeRebind (Rust::AST::UseTreeRebind::NONE, bul,
				  UNDEF_LOCATION));

  auto uses = std::vector<std::unique_ptr<Rust::AST::UseTree>> ();
  uses.emplace_back (std::move (use0));
  uses.emplace_back (std::move (use1));

  auto list
    = Rust::AST::UseTreeList (Rust::AST::UseTreeList::PATH_PREFIXED, foo_bar,
			      std::move (uses), UNDEF_LOCATION);

  auto imports = std::vector<Rust::Resolver::Import> ();
  Rust::Resolver::flatten_list (list, imports);

  ASSERT_TRUE (!imports.empty ());
  ASSERT_EQ (imports.size (), 2);
  ASSERT_EQ (imports[0].get_path ().get_segments ()[0].as_string (), "foo");
  ASSERT_EQ (imports[0].get_path ().get_segments ()[1].as_string (), "bar");
  ASSERT_EQ (imports[0].get_path ().get_segments ()[2].as_string (), "baz");
  ASSERT_EQ (imports[1].get_path ().get_segments ()[0].as_string (), "foo");
  ASSERT_EQ (imports[1].get_path ().get_segments ()[1].as_string (), "bar");
  ASSERT_EQ (imports[1].get_path ().get_segments ()[2].as_string (), "bul");
}

static void
rust_use_dec_flattening (void)
{
  rust_flatten_glob ();
  rust_flatten_nested_glob ();
  rust_flatten_rebind_none ();
  rust_flatten_rebind ();
  rust_flatten_rebind_nested ();
  rust_flatten_list ();
}

void
rust_simple_path_resolve_test (void)
{
  rust_use_dec_flattening ();
}

} // namespace selftest

#endif // CHECKING_P
