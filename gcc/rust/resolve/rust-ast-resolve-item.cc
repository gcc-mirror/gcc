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

#include "rust-ast-resolve-item.h"

namespace Rust {
namespace Resolver {
void
ResolveTraitItems::go (AST::TraitItem *item, const CanonicalPath &prefix,
		       const CanonicalPath &canonical_prefix)
{
  if (item->is_marked_for_strip ())
    return;

  ResolveTraitItems resolver (prefix, canonical_prefix);
  item->accept_vis (resolver);
}

void
ResolveTraitItems::visit (AST::TraitItemType &type)
{
  auto decl = ResolveTraitItemTypeToCanonicalPath::resolve (type);
  auto path = prefix.append (decl);
  auto cpath = canonical_prefix.append (decl);
  mappings->insert_canonical_path (mappings->get_current_crate (),
				   type.get_node_id (), cpath);

  for (auto &bound : type.get_type_param_bounds ())
    ResolveTypeBound::go (bound.get (), type.get_node_id ());
}

void
ResolveTraitItems::visit (AST::TraitItemFunc &func)
{
  auto decl = ResolveTraitItemFunctionToCanonicalPath::resolve (func);
  auto path = prefix.append (decl);
  auto cpath = canonical_prefix.append (decl);
  mappings->insert_canonical_path (mappings->get_current_crate (),
				   func.get_node_id (), cpath);

  NodeId scope_node_id = func.get_node_id ();
  resolver->get_name_scope ().push (scope_node_id);
  resolver->get_type_scope ().push (scope_node_id);
  resolver->get_label_scope ().push (scope_node_id);
  resolver->push_new_name_rib (resolver->get_name_scope ().peek ());
  resolver->push_new_type_rib (resolver->get_type_scope ().peek ());
  resolver->push_new_label_rib (resolver->get_type_scope ().peek ());

  AST::TraitFunctionDecl &function = func.get_trait_function_decl ();
  if (function.has_generics ())
    {
      for (auto &generic : function.get_generic_params ())
	ResolveGenericParam::go (generic.get (), func.get_node_id ());
    }

  if (function.has_return_type ())
    ResolveType::go (function.get_return_type ().get (), func.get_node_id ());

  // we make a new scope so the names of parameters are resolved and shadowed
  // correctly
  for (auto &param : function.get_function_params ())
    {
      ResolveType::go (param.get_type ().get (), param.get_node_id ());
      PatternDeclaration::go (param.get_pattern ().get (),
			      param.get_node_id ());

      // the mutability checker needs to verify for immutable decls the number
      // of assignments are <1. This marks an implicit assignment
      resolver->mark_assignment_to_decl (
	param.get_pattern ()->get_pattern_node_id (), param.get_node_id ());
    }

  if (function.has_where_clause ())
    ResolveWhereClause::Resolve (function.get_where_clause ());

  // trait items have an optional body
  if (func.has_definition ())
    ResolveExpr::go (func.get_definition ().get (), func.get_node_id (), path,
		     cpath);

  resolver->get_name_scope ().pop ();
  resolver->get_type_scope ().pop ();
  resolver->get_label_scope ().pop ();
}

void
ResolveTraitItems::visit (AST::TraitItemMethod &func)
{
  auto decl = ResolveTraitItemMethodToCanonicalPath::resolve (func);
  auto path = prefix.append (decl);
  auto cpath = canonical_prefix.append (decl);
  mappings->insert_canonical_path (mappings->get_current_crate (),
				   func.get_node_id (), cpath);

  NodeId scope_node_id = func.get_node_id ();
  resolver->get_name_scope ().push (scope_node_id);
  resolver->get_type_scope ().push (scope_node_id);
  resolver->get_label_scope ().push (scope_node_id);
  resolver->push_new_name_rib (resolver->get_name_scope ().peek ());
  resolver->push_new_type_rib (resolver->get_type_scope ().peek ());
  resolver->push_new_label_rib (resolver->get_type_scope ().peek ());

  AST::TraitMethodDecl &function = func.get_trait_method_decl ();
  if (function.has_generics ())
    {
      for (auto &generic : function.get_generic_params ())
	ResolveGenericParam::go (generic.get (), func.get_node_id ());
    }

  if (function.has_return_type ())
    ResolveType::go (function.get_return_type ().get (), func.get_node_id ());

  // self turns into (self: Self) as a function param
  AST::SelfParam &self_param = function.get_self_param ();
  AST::IdentifierPattern self_pattern (self_param.get_node_id (), "self",
				       self_param.get_locus (),
				       self_param.get_has_ref (),
				       self_param.get_is_mut (),
				       std::unique_ptr<AST::Pattern> (nullptr));

  std::vector<std::unique_ptr<AST::TypePathSegment> > segments;
  segments.push_back (std::unique_ptr<AST::TypePathSegment> (
    new AST::TypePathSegment ("Self", false, self_param.get_locus ())));

  AST::TypePath self_type_path (std::move (segments), self_param.get_locus ());

  ResolveType::go (&self_type_path, self_param.get_node_id ());
  PatternDeclaration::go (&self_pattern, self_param.get_node_id ());

  resolver->mark_assignment_to_decl (self_pattern.get_node_id (),
				     self_pattern.get_node_id ());

  // we make a new scope so the names of parameters are resolved and shadowed
  // correctly
  for (auto &param : function.get_function_params ())
    {
      ResolveType::go (param.get_type ().get (), param.get_node_id ());
      PatternDeclaration::go (param.get_pattern ().get (),
			      param.get_node_id ());

      // the mutability checker needs to verify for immutable decls the number
      // of assignments are <1. This marks an implicit assignment
      resolver->mark_assignment_to_decl (
	param.get_pattern ()->get_pattern_node_id (), param.get_node_id ());
    }

  if (function.has_where_clause ())
    ResolveWhereClause::Resolve (function.get_where_clause ());

  // trait items have an optional body
  if (func.has_definition ())
    ResolveExpr::go (func.get_definition ().get (), func.get_node_id (), path,
		     cpath);

  resolver->get_name_scope ().pop ();
  resolver->get_type_scope ().pop ();
  resolver->get_label_scope ().pop ();
}

void
ResolveTraitItems::visit (AST::TraitItemConst &constant)
{
  auto decl = ResolveTraitItemConstToCanonicalPath::resolve (constant);
  auto path = prefix.append (decl);
  auto cpath = canonical_prefix.append (decl);
  mappings->insert_canonical_path (mappings->get_current_crate (),
				   constant.get_node_id (), cpath);

  ResolveType::go (constant.get_type ().get (), constant.get_node_id ());

  if (constant.has_expr ())
    ResolveExpr::go (constant.get_expr ().get (), constant.get_node_id (), path,
		     cpath);

  // the mutability checker needs to verify for immutable decls the number
  // of assignments are <1. This marks an implicit assignment
  resolver->mark_decl_mutability (constant.get_node_id (), false);
  resolver->mark_assignment_to_decl (constant.get_node_id (),
				     constant.get_node_id ());
}

void
ResolveItem::go (AST::Item *item, const CanonicalPath &prefix,
		 const CanonicalPath &canonical_prefix)
{
  ResolveItem resolver (prefix, canonical_prefix);
  item->accept_vis (resolver);
}

void
ResolveItem::visit (AST::TypeAlias &alias)
{
  auto talias
    = CanonicalPath::new_seg (alias.get_node_id (), alias.get_new_type_name ());
  auto path = prefix.append (talias);
  auto cpath = canonical_prefix.append (talias);
  mappings->insert_canonical_path (mappings->get_current_crate (),
				   alias.get_node_id (), cpath);

  NodeId scope_node_id = alias.get_node_id ();
  resolver->get_type_scope ().push (scope_node_id);

  if (alias.has_generics ())
    {
      for (auto &generic : alias.get_generic_params ())
	ResolveGenericParam::go (generic.get (), alias.get_node_id ());
    }

  if (alias.has_where_clause ())
    ResolveWhereClause::Resolve (alias.get_where_clause ());

  ResolveType::go (alias.get_type_aliased ().get (), alias.get_node_id ());

  resolver->get_type_scope ().pop ();
}

void
ResolveItem::visit (AST::Module &module)
{
  auto mod = CanonicalPath::new_seg (module.get_node_id (), module.get_name ());
  auto path = prefix.append (mod);
  auto cpath = canonical_prefix.append (mod);
  mappings->insert_canonical_path (mappings->get_current_crate (),
				   module.get_node_id (), cpath);

  NodeId scope_node_id = module.get_node_id ();
  resolver->get_name_scope ().push (scope_node_id);
  resolver->get_type_scope ().push (scope_node_id);
  resolver->get_label_scope ().push (scope_node_id);
  resolver->push_new_name_rib (resolver->get_name_scope ().peek ());
  resolver->push_new_type_rib (resolver->get_type_scope ().peek ());
  resolver->push_new_label_rib (resolver->get_type_scope ().peek ());

  for (auto &item : module.get_items ())
    ResolveTopLevel::go (item.get (), CanonicalPath::create_empty (), cpath);

  for (auto &item : module.get_items ())
    ResolveItem::go (item.get (), path, cpath);

  resolver->get_name_scope ().pop ();
  resolver->get_type_scope ().pop ();
  resolver->get_label_scope ().pop ();
}

void
ResolveItem::visit (AST::TupleStruct &struct_decl)
{
  auto decl = CanonicalPath::new_seg (struct_decl.get_node_id (),
				      struct_decl.get_identifier ());
  auto path = prefix.append (decl);
  auto cpath = canonical_prefix.append (decl);
  mappings->insert_canonical_path (mappings->get_current_crate (),
				   struct_decl.get_node_id (), cpath);

  NodeId scope_node_id = struct_decl.get_node_id ();
  resolver->get_type_scope ().push (scope_node_id);

  if (struct_decl.has_generics ())
    {
      for (auto &generic : struct_decl.get_generic_params ())
	{
	  ResolveGenericParam::go (generic.get (), struct_decl.get_node_id ());
	}
    }

  if (struct_decl.has_where_clause ())
    ResolveWhereClause::Resolve (struct_decl.get_where_clause ());

  for (AST::TupleField &field : struct_decl.get_fields ())
    {
      if (field.get_field_type ()->is_marked_for_strip ())
	continue;

      ResolveType::go (field.get_field_type ().get (),
		       struct_decl.get_node_id ());
    }

  resolver->get_type_scope ().pop ();
}

void
ResolveItem::visit (AST::Enum &enum_decl)
{
  auto decl = CanonicalPath::new_seg (enum_decl.get_node_id (),
				      enum_decl.get_identifier ());
  auto path = prefix.append (decl);
  auto cpath = canonical_prefix.append (decl);
  mappings->insert_canonical_path (mappings->get_current_crate (),
				   enum_decl.get_node_id (), cpath);

  NodeId scope_node_id = enum_decl.get_node_id ();
  resolver->get_type_scope ().push (scope_node_id);

  if (enum_decl.has_generics ())
    {
      for (auto &generic : enum_decl.get_generic_params ())
	{
	  ResolveGenericParam::go (generic.get (), enum_decl.get_node_id ());
	}
    }

  if (enum_decl.has_where_clause ())
    ResolveWhereClause::Resolve (enum_decl.get_where_clause ());

  /* The actual fields are inside the variants.  */
  for (auto &variant : enum_decl.get_variants ())
    ResolveItem::go (variant.get (), path, cpath);

  resolver->get_type_scope ().pop ();
}

/* EnumItem doesn't need to be handled, no fields.  */
void
ResolveItem::visit (AST::EnumItem &item)
{
  auto decl
    = CanonicalPath::new_seg (item.get_node_id (), item.get_identifier ());
  auto path = prefix.append (decl);
  auto cpath = canonical_prefix.append (decl);
  mappings->insert_canonical_path (mappings->get_current_crate (),
				   item.get_node_id (), cpath);
}

void
ResolveItem::visit (AST::EnumItemTuple &item)
{
  auto decl
    = CanonicalPath::new_seg (item.get_node_id (), item.get_identifier ());
  auto path = prefix.append (decl);
  auto cpath = canonical_prefix.append (decl);
  mappings->insert_canonical_path (mappings->get_current_crate (),
				   item.get_node_id (), cpath);

  for (auto &field : item.get_tuple_fields ())
    {
      if (field.get_field_type ()->is_marked_for_strip ())
	continue;

      ResolveType::go (field.get_field_type ().get (), item.get_node_id ());
    }
}

void
ResolveItem::visit (AST::EnumItemStruct &item)
{
  auto decl
    = CanonicalPath::new_seg (item.get_node_id (), item.get_identifier ());
  auto path = prefix.append (decl);
  auto cpath = canonical_prefix.append (decl);
  mappings->insert_canonical_path (mappings->get_current_crate (),
				   item.get_node_id (), cpath);

  for (auto &field : item.get_struct_fields ())
    {
      if (field.get_field_type ()->is_marked_for_strip ())
	continue;

      ResolveType::go (field.get_field_type ().get (), item.get_node_id ());
    }
}

void
ResolveItem::visit (AST::EnumItemDiscriminant &item)
{
  auto decl
    = CanonicalPath::new_seg (item.get_node_id (), item.get_identifier ());
  auto path = prefix.append (decl);
  auto cpath = canonical_prefix.append (decl);

  mappings->insert_canonical_path (mappings->get_current_crate (),
				   item.get_node_id (), cpath);
}

void
ResolveItem::visit (AST::StructStruct &struct_decl)
{
  auto decl = CanonicalPath::new_seg (struct_decl.get_node_id (),
				      struct_decl.get_identifier ());
  auto path = prefix.append (decl);
  auto cpath = canonical_prefix.append (decl);
  mappings->insert_canonical_path (mappings->get_current_crate (),
				   struct_decl.get_node_id (), cpath);

  NodeId scope_node_id = struct_decl.get_node_id ();
  resolver->get_type_scope ().push (scope_node_id);

  if (struct_decl.has_generics ())
    {
      for (auto &generic : struct_decl.get_generic_params ())
	{
	  ResolveGenericParam::go (generic.get (), struct_decl.get_node_id ());
	}
    }

  if (struct_decl.has_where_clause ())
    ResolveWhereClause::Resolve (struct_decl.get_where_clause ());

  for (AST::StructField &field : struct_decl.get_fields ())
    {
      if (field.get_field_type ()->is_marked_for_strip ())
	continue;

      ResolveType::go (field.get_field_type ().get (),
		       struct_decl.get_node_id ());
    }

  resolver->get_type_scope ().pop ();
}

void
ResolveItem::visit (AST::Union &union_decl)
{
  auto decl = CanonicalPath::new_seg (union_decl.get_node_id (),
				      union_decl.get_identifier ());
  auto path = prefix.append (decl);
  auto cpath = canonical_prefix.append (decl);
  mappings->insert_canonical_path (mappings->get_current_crate (),
				   union_decl.get_node_id (), cpath);

  NodeId scope_node_id = union_decl.get_node_id ();
  resolver->get_type_scope ().push (scope_node_id);

  if (union_decl.has_generics ())
    {
      for (auto &generic : union_decl.get_generic_params ())
	{
	  ResolveGenericParam::go (generic.get (), union_decl.get_node_id ());
	}
    }

  if (union_decl.has_where_clause ())
    ResolveWhereClause::Resolve (union_decl.get_where_clause ());

  for (AST::StructField &field : union_decl.get_variants ())
    {
      if (field.get_field_type ()->is_marked_for_strip ())
	continue;

      ResolveType::go (field.get_field_type ().get (),
		       union_decl.get_node_id ());
    }

  resolver->get_type_scope ().pop ();
}

void
ResolveItem::visit (AST::StaticItem &var)
{
  auto decl
    = CanonicalPath::new_seg (var.get_node_id (), var.get_identifier ());
  auto path = prefix.append (decl);
  auto cpath = canonical_prefix.append (decl);
  mappings->insert_canonical_path (mappings->get_current_crate (),
				   var.get_node_id (), cpath);

  ResolveType::go (var.get_type ().get (), var.get_node_id ());
  ResolveExpr::go (var.get_expr ().get (), var.get_node_id (), path, cpath);

  // the mutability checker needs to verify for immutable decls the number
  // of assignments are <1. This marks an implicit assignment
  resolver->mark_assignment_to_decl (var.get_node_id (), var.get_node_id ());
}

void
ResolveItem::visit (AST::ConstantItem &constant)
{
  auto decl = ResolveConstantItemToCanonicalPath::resolve (constant);
  auto path = prefix.append (decl);
  auto cpath = canonical_prefix.append (decl);
  mappings->insert_canonical_path (mappings->get_current_crate (),
				   constant.get_node_id (), cpath);

  ResolveType::go (constant.get_type ().get (), constant.get_node_id ());
  ResolveExpr::go (constant.get_expr ().get (), constant.get_node_id (), path,
		   cpath);

  // the mutability checker needs to verify for immutable decls the number
  // of assignments are <1. This marks an implicit assignment
  resolver->mark_decl_mutability (constant.get_node_id (), false);
  resolver->mark_assignment_to_decl (constant.get_node_id (),
				     constant.get_node_id ());
}

void
ResolveItem::visit (AST::Function &function)
{
  auto decl = ResolveFunctionItemToCanonicalPath::resolve (function);
  auto path = prefix.append (decl);
  auto cpath = canonical_prefix.append (decl);
  mappings->insert_canonical_path (mappings->get_current_crate (),
				   function.get_node_id (), cpath);

  NodeId scope_node_id = function.get_node_id ();
  resolver->get_name_scope ().push (scope_node_id);
  resolver->get_type_scope ().push (scope_node_id);
  resolver->get_label_scope ().push (scope_node_id);
  resolver->push_new_name_rib (resolver->get_name_scope ().peek ());
  resolver->push_new_type_rib (resolver->get_type_scope ().peek ());
  resolver->push_new_label_rib (resolver->get_type_scope ().peek ());

  if (function.has_generics ())
    {
      for (auto &generic : function.get_generic_params ())
	ResolveGenericParam::go (generic.get (), function.get_node_id ());
    }

  // resolve any where clause items
  if (function.has_where_clause ())
    ResolveWhereClause::Resolve (function.get_where_clause ());

  if (function.has_return_type ())
    ResolveType::go (function.get_return_type ().get (),
		     function.get_node_id ());

  // we make a new scope so the names of parameters are resolved and shadowed
  // correctly
  for (auto &param : function.get_function_params ())
    {
      ResolveType::go (param.get_type ().get (), param.get_node_id ());
      PatternDeclaration::go (param.get_pattern ().get (),
			      param.get_node_id ());

      // the mutability checker needs to verify for immutable decls the number
      // of assignments are <1. This marks an implicit assignment
      resolver->mark_assignment_to_decl (
	param.get_pattern ()->get_pattern_node_id (), param.get_node_id ());
    }

  // resolve the function body
  ResolveExpr::go (function.get_definition ().get (), function.get_node_id (),
		   path, cpath);

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

  if (impl_block.has_generics ())
    {
      for (auto &generic : impl_block.get_generic_params ())
	{
	  ResolveGenericParam::go (generic.get (), impl_block.get_node_id ());
	}
    }

  // resolve any where clause items
  if (impl_block.has_where_clause ())
    ResolveWhereClause::Resolve (impl_block.get_where_clause ());

  // FIXME this needs to be protected behind nominal type-checks see:
  // rustc --explain E0118

  CanonicalPath self_cpath = CanonicalPath::create_empty ();
  bool canonicalize_type_with_generics = false;
  NodeId resolved_node
    = ResolveType::go (impl_block.get_type ().get (), impl_block.get_node_id (),
		       canonicalize_type_with_generics, &self_cpath);
  if (resolved_node == UNKNOWN_NODEID)
    {
      resolver->get_type_scope ().pop ();
      resolver->get_name_scope ().pop ();
      return;
    }

  // Setup paths
  bool canonicalize_type_args = !impl_block.has_generics ();
  bool type_resolve_generic_args = false;

  CanonicalPath impl_type
    = ResolveTypeToCanonicalPath::resolve (*impl_block.get_type ().get (),
					   canonicalize_type_args,
					   type_resolve_generic_args);
  CanonicalPath impl_prefix = prefix.append (impl_type);

  // see https://godbolt.org/z/a3vMbsT6W
  CanonicalPath cpath = CanonicalPath::create_empty ();
  if (canonical_prefix.size () <= 1)
    {
      cpath = self_cpath;
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
    = CanonicalPath::get_big_self (impl_block.get_type ()->get_node_id ());

  resolver->get_type_scope ().insert (Self,
				      impl_block.get_type ()->get_node_id (),
				      impl_block.get_type ()->get_locus ());

  for (auto &impl_item : impl_block.get_impl_items ())
    {
      resolve_impl_item (impl_item.get (), impl_prefix, cpath);
    }

  resolver->get_type_scope ().peek ()->clear_name (
    Self, impl_block.get_type ()->get_node_id ());

  resolver->get_type_scope ().pop ();
  resolver->get_name_scope ().pop ();
}

void
ResolveItem::visit (AST::Method &method)
{
  auto decl = ResolveMethodItemToCanonicalPath::resolve (method);
  auto path = prefix.append (decl);
  auto cpath = canonical_prefix.append (decl);
  mappings->insert_canonical_path (mappings->get_current_crate (),
				   method.get_node_id (), cpath);

  NodeId scope_node_id = method.get_node_id ();
  resolver->get_name_scope ().push (scope_node_id);
  resolver->get_type_scope ().push (scope_node_id);
  resolver->get_label_scope ().push (scope_node_id);
  resolver->push_new_name_rib (resolver->get_name_scope ().peek ());
  resolver->push_new_type_rib (resolver->get_type_scope ().peek ());
  resolver->push_new_label_rib (resolver->get_type_scope ().peek ());

  if (method.has_generics ())
    {
      for (auto &generic : method.get_generic_params ())
	ResolveGenericParam::go (generic.get (), method.get_node_id ());
    }

  // resolve any where clause items
  if (method.has_where_clause ())
    ResolveWhereClause::Resolve (method.get_where_clause ());

  if (method.has_return_type ())
    ResolveType::go (method.get_return_type ().get (), method.get_node_id ());

  // self turns into (self: Self) as a function param
  AST::SelfParam &self_param = method.get_self_param ();
  AST::IdentifierPattern self_pattern (self_param.get_node_id (), "self",
				       self_param.get_locus (),
				       self_param.get_has_ref (),
				       self_param.get_is_mut (),
				       std::unique_ptr<AST::Pattern> (nullptr));

  std::vector<std::unique_ptr<AST::TypePathSegment> > segments;
  segments.push_back (std::unique_ptr<AST::TypePathSegment> (
    new AST::TypePathSegment ("Self", false, self_param.get_locus ())));

  AST::TypePath self_type_path (std::move (segments), self_param.get_locus ());

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
      resolver->mark_assignment_to_decl (
	param.get_pattern ()->get_pattern_node_id (), param.get_node_id ());
    }

  // resolve any where clause items
  if (method.has_where_clause ())
    ResolveWhereClause::Resolve (method.get_where_clause ());

  // resolve the function body
  ResolveExpr::go (method.get_definition ().get (), method.get_node_id (), path,
		   cpath);

  resolver->get_name_scope ().pop ();
  resolver->get_type_scope ().pop ();
  resolver->get_label_scope ().pop ();
}

void
ResolveItem::visit (AST::TraitImpl &impl_block)
{
  NodeId scope_node_id = impl_block.get_node_id ();
  resolver->get_name_scope ().push (scope_node_id);
  resolver->get_type_scope ().push (scope_node_id);
  resolver->push_new_name_rib (resolver->get_name_scope ().peek ());
  resolver->push_new_type_rib (resolver->get_type_scope ().peek ());

  if (impl_block.has_generics ())
    {
      for (auto &generic : impl_block.get_generic_params ())
	{
	  ResolveGenericParam::go (generic.get (), impl_block.get_node_id ());
	}
    }

  // resolve any where clause items
  if (impl_block.has_where_clause ())
    ResolveWhereClause::Resolve (impl_block.get_where_clause ());

  CanonicalPath canonical_trait_type = CanonicalPath::create_empty ();
  bool canonicalize_type_with_generics = false;
  NodeId trait_resolved_node
    = ResolveType::go (&impl_block.get_trait_path (), impl_block.get_node_id (),
		       canonicalize_type_with_generics, &canonical_trait_type);
  if (trait_resolved_node == UNKNOWN_NODEID)
    {
      resolver->get_type_scope ().pop ();
      resolver->get_name_scope ().pop ();
      return;
    }

  CanonicalPath canonical_impl_type = CanonicalPath::create_empty ();
  NodeId type_resolved_node
    = ResolveType::go (impl_block.get_type ().get (), impl_block.get_node_id (),
		       canonicalize_type_with_generics, &canonical_impl_type);
  if (type_resolved_node == UNKNOWN_NODEID)
    {
      resolver->get_type_scope ().pop ();
      resolver->get_name_scope ().pop ();
      return;
    }

  // setup paths
  bool canonicalize_type_args = !impl_block.has_generics ();
  bool type_resolve_generic_args = false;

  CanonicalPath impl_type_seg
    = ResolveTypeToCanonicalPath::resolve (*impl_block.get_type ().get (),
					   canonicalize_type_args,
					   type_resolve_generic_args);
  CanonicalPath trait_type_seg
    = ResolveTypeToCanonicalPath::resolve (impl_block.get_trait_path (),
					   canonicalize_type_args,
					   type_resolve_generic_args);

  CanonicalPath projection
    = TraitImplProjection::resolve (impl_block.get_node_id (), trait_type_seg,
				    impl_type_seg);
  CanonicalPath impl_prefix = prefix.append (projection);

  // setup canonical-path
  CanonicalPath canonical_projection
    = TraitImplProjection::resolve (impl_block.get_node_id (),
				    canonical_trait_type, canonical_impl_type);
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
    = CanonicalPath::get_big_self (impl_block.get_type ()->get_node_id ());

  resolver->get_type_scope ().insert (Self,
				      impl_block.get_type ()->get_node_id (),
				      impl_block.get_type ()->get_locus ());

  for (auto &impl_item : impl_block.get_impl_items ())
    {
      resolve_impl_item (impl_item.get (), impl_prefix, cpath);
    }

  resolver->get_type_scope ().peek ()->clear_name (
    Self, impl_block.get_type ()->get_node_id ());
  resolver->get_type_scope ().pop ();
}

void
ResolveItem::visit (AST::Trait &trait)
{
  NodeId scope_node_id = trait.get_node_id ();
  resolver->get_name_scope ().push (scope_node_id);
  resolver->get_type_scope ().push (scope_node_id);
  resolver->push_new_name_rib (resolver->get_name_scope ().peek ());
  resolver->push_new_type_rib (resolver->get_type_scope ().peek ());

  // we need to inject an implicit self TypeParam here
  AST::TypeParam *implicit_self
    = new AST::TypeParam ("Self", trait.get_locus ());
  trait.insert_implict_self (
    std::unique_ptr<AST::GenericParam> (implicit_self));
  CanonicalPath Self = CanonicalPath::get_big_self (trait.get_node_id ());

  for (auto &generic : trait.get_generic_params ())
    {
      ResolveGenericParam::go (generic.get (), trait.get_node_id ());
    }

  // Self is an implicit TypeParam so lets mark it as such
  resolver->get_type_scope ().append_reference_for_def (
    Self.get_node_id (), implicit_self->get_node_id ());

  if (trait.has_type_param_bounds ())
    {
      for (auto &bound : trait.get_type_param_bounds ())
	{
	  ResolveTypeBound::go (bound.get (), trait.get_node_id ());
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
  for (auto &item : extern_block.get_extern_items ())
    {
      resolve_extern_item (item.get ());
    }
}

void
ResolveImplItems::go (AST::InherentImplItem *item, const CanonicalPath &prefix,
		      const CanonicalPath &canonical_prefix)
{
  if (item->is_marked_for_strip ())
    return;

  ResolveImplItems resolver (prefix, canonical_prefix);
  item->accept_vis (resolver);
}

void
ResolveImplItems::go (AST::TraitImplItem *item, const CanonicalPath &prefix,
		      const CanonicalPath &canonical_prefix)
{
  if (item->is_marked_for_strip ())
    return;

  ResolveImplItems resolver (prefix, canonical_prefix);
  item->accept_vis (resolver);
}

void
ResolveImplItems::visit (AST::TypeAlias &alias)
{
  ResolveItem::visit (alias);

  // FIXME this stops the erronious unused decls which will be fixed later on
  resolver->get_type_scope ().append_reference_for_def (alias.get_node_id (),
							alias.get_node_id ());
}

void
ResolveExternItem::go (AST::ExternalItem *item)
{
  ResolveExternItem resolver;
  item->accept_vis (resolver);
}

void
ResolveExternItem::visit (AST::ExternalFunctionItem &function)
{
  NodeId scope_node_id = function.get_node_id ();
  resolver->get_name_scope ().push (scope_node_id);
  resolver->get_type_scope ().push (scope_node_id);
  resolver->get_label_scope ().push (scope_node_id);
  resolver->push_new_name_rib (resolver->get_name_scope ().peek ());
  resolver->push_new_type_rib (resolver->get_type_scope ().peek ());
  resolver->push_new_label_rib (resolver->get_type_scope ().peek ());

  // resolve the generics
  if (function.has_generics ())
    {
      for (auto &generic : function.get_generic_params ())
	ResolveGenericParam::go (generic.get (), function.get_node_id ());
    }

  if (function.has_return_type ())
    ResolveType::go (function.get_return_type ().get (),
		     function.get_node_id ());

  // we make a new scope so the names of parameters are resolved and shadowed
  // correctly
  for (auto &param : function.get_function_params ())
    {
      ResolveType::go (param.get_type ().get (), param.get_node_id ());
    }

  // done
  resolver->get_name_scope ().pop ();
  resolver->get_type_scope ().pop ();
  resolver->get_label_scope ().pop ();
}

void
ResolveExternItem::visit (AST::ExternalStaticItem &item)
{
  ResolveType::go (item.get_type ().get (), item.get_node_id ());
}

} // namespace Resolver
} // namespace Rust
