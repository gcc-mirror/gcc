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

#include "rust-hir-item.h"
#include "optional.h"

namespace Rust {
namespace HIR {

TypeParam::TypeParam (
  Analysis::NodeMapping mappings, Identifier type_representation,
  location_t locus,
  std::vector<std::unique_ptr<TypeParamBound>> type_param_bounds,
  tl::optional<std::unique_ptr<Type>> type, AST::AttrVec outer_attrs)
  : GenericParam (mappings), outer_attrs (std::move (outer_attrs)),
    type_representation (std::move (type_representation)),
    type_param_bounds (std::move (type_param_bounds)), type (std::move (type)),
    locus (locus)
{}

TypeParam::TypeParam (TypeParam const &other)
  : GenericParam (other.mappings), outer_attrs (other.outer_attrs),
    type_representation (other.type_representation), locus (other.locus)
{
  // guard to prevent null pointer dereference
  if (other.has_type ())
    type = {other.type.value ()->clone_type ()};
  else
    type = tl::nullopt;

  type_param_bounds.reserve (other.type_param_bounds.size ());
  for (const auto &e : other.type_param_bounds)
    type_param_bounds.push_back (e->clone_type_param_bound ());
}

TypeParam &
TypeParam::operator= (TypeParam const &other)
{
  type_representation = other.type_representation;
  outer_attrs = other.outer_attrs;
  locus = other.locus;
  mappings = other.mappings;

  // guard to prevent null pointer dereference
  if (other.has_type ())
    type = {other.type.value ()->clone_type ()};
  else
    type = tl::nullopt;

  type_param_bounds.reserve (other.type_param_bounds.size ());
  for (const auto &e : other.type_param_bounds)
    type_param_bounds.push_back (e->clone_type_param_bound ());

  return *this;
}

Analysis::NodeMapping
TypeParam::get_type_mappings () const
{
  rust_assert (type.has_value ());
  return type.value ()->get_mappings ();
}

std::vector<std::unique_ptr<TypeParamBound>> &
TypeParam::get_type_param_bounds ()
{
  return type_param_bounds;
}

TypeBoundWhereClauseItem::TypeBoundWhereClauseItem (
  Analysis::NodeMapping mappings, std::vector<LifetimeParam> for_lifetimes,
  std::unique_ptr<Type> bound_type,
  std::vector<std::unique_ptr<TypeParamBound>> type_param_bounds,
  location_t locus)
  : for_lifetimes (std::move (for_lifetimes)),
    bound_type (std::move (bound_type)),
    type_param_bounds (std::move (type_param_bounds)),
    mappings (std::move (mappings)), locus (locus)
{}

TypeBoundWhereClauseItem::TypeBoundWhereClauseItem (
  TypeBoundWhereClauseItem const &other)
  : for_lifetimes (other.for_lifetimes),
    bound_type (other.bound_type->clone_type ()), mappings (other.mappings)
{
  type_param_bounds.reserve (other.type_param_bounds.size ());
  for (const auto &e : other.type_param_bounds)
    type_param_bounds.push_back (e->clone_type_param_bound ());
}

TypeBoundWhereClauseItem &
TypeBoundWhereClauseItem::operator= (TypeBoundWhereClauseItem const &other)
{
  mappings = other.mappings;
  for_lifetimes = other.for_lifetimes;
  bound_type = other.bound_type->clone_type ();
  type_param_bounds.reserve (other.type_param_bounds.size ());
  for (const auto &e : other.type_param_bounds)
    type_param_bounds.push_back (e->clone_type_param_bound ());

  return *this;
}

std::vector<std::unique_ptr<TypeParamBound>> &
TypeBoundWhereClauseItem::get_type_param_bounds ()
{
  return type_param_bounds;
}

SelfParam::SelfParam (Analysis::NodeMapping mappings,
		      ImplicitSelfKind self_kind,
		      tl::optional<Lifetime> lifetime, Type *type)
  : self_kind (self_kind), lifetime (std::move (lifetime)), type (type),
    mappings (mappings)
{}

SelfParam::SelfParam (Analysis::NodeMapping mappings,
		      std::unique_ptr<Type> type, bool is_mut, location_t locus)
  : self_kind (is_mut ? ImplicitSelfKind::MUT : ImplicitSelfKind::IMM),
    lifetime (tl::nullopt), type (std::move (type)), locus (locus),
    mappings (mappings)
{}

SelfParam::SelfParam (Analysis::NodeMapping mappings,
		      tl::optional<Lifetime> lifetime, bool is_mut,
		      location_t locus)
  : self_kind (is_mut ? ImplicitSelfKind::MUT_REF : ImplicitSelfKind::IMM_REF),
    lifetime (std::move (lifetime)), locus (locus), mappings (mappings)
{}

SelfParam::SelfParam (SelfParam const &other)
  : self_kind (other.self_kind), lifetime (other.lifetime), locus (other.locus),
    mappings (other.mappings)
{
  if (other.type != nullptr)
    type = other.type->clone_type ();
}

SelfParam &
SelfParam::operator= (SelfParam const &other)
{
  if (other.type != nullptr)
    type = other.type->clone_type ();

  self_kind = other.self_kind;
  lifetime = other.lifetime;
  locus = other.locus;
  mappings = other.mappings;

  return *this;
}

Mutability
SelfParam::get_mut () const
{
  return (self_kind == ImplicitSelfKind::MUT
	  || self_kind == ImplicitSelfKind::MUT_REF)
	   ? Mutability::Mut
	   : Mutability::Imm;
}

bool
SelfParam::is_mut () const
{
  return self_kind == ImplicitSelfKind::MUT
	 || self_kind == ImplicitSelfKind::MUT_REF;
}

bool
SelfParam::is_ref () const
{
  return self_kind == ImplicitSelfKind::IMM_REF
	 || self_kind == ImplicitSelfKind::MUT_REF;
}

FunctionParam::FunctionParam (Analysis::NodeMapping mappings,
			      std::unique_ptr<Pattern> param_name,
			      std::unique_ptr<Type> param_type,
			      location_t locus)
  : param_name (std::move (param_name)), type (std::move (param_type)),
    locus (locus), mappings (mappings)
{}

FunctionParam::FunctionParam (FunctionParam const &other)
  : param_name (other.param_name->clone_pattern ()),
    type (other.type->clone_type ()), locus (other.locus),
    mappings (other.mappings)
{}

FunctionParam &
FunctionParam::operator= (FunctionParam const &other)
{
  param_name = other.param_name->clone_pattern ();
  type = other.type->clone_type ();
  locus = other.locus;
  mappings = other.mappings;

  return *this;
}

VisItem &
VisItem::operator= (VisItem const &other)
{
  Item::operator= (other);
  visibility = other.visibility;
  // outer_attrs = other.outer_attrs;

  return *this;
}

VisItem::VisItem (VisItem const &other)
  : Item (other), visibility (other.visibility)
{}

Module::Module (Analysis::NodeMapping mappings, Identifier module_name,
		location_t locus, std::vector<std::unique_ptr<Item>> items,
		Visibility visibility, AST::AttrVec inner_attrs,
		AST::AttrVec outer_attrs)
  : VisItem (std::move (mappings), std::move (visibility),
	     std::move (outer_attrs)),
    WithInnerAttrs (std::move (inner_attrs)), module_name (module_name),
    locus (locus), items (std::move (items))
{}

Module::Module (Module const &other)
  : VisItem (other), WithInnerAttrs (other.inner_attrs), module_name ("")
{
  items.reserve (other.items.size ());
  for (const auto &e : other.items)
    items.push_back (e->clone_item ());
}

Module &
Module::operator= (Module const &other)
{
  VisItem::operator= (other);
  inner_attrs = other.inner_attrs;

  items.reserve (other.items.size ());
  for (const auto &e : other.items)
    items.push_back (e->clone_item ());

  return *this;
}

Function::Function (Analysis::NodeMapping mappings, Identifier function_name,
		    FunctionQualifiers qualifiers,
		    std::vector<std::unique_ptr<GenericParam>> generic_params,
		    std::vector<FunctionParam> function_params,
		    std::unique_ptr<Type> return_type, WhereClause where_clause,
		    std::unique_ptr<BlockExpr> function_body, Visibility vis,
		    AST::AttrVec outer_attrs, tl::optional<SelfParam> self,
		    Defaultness defaultness, location_t locus)
  : VisItem (std::move (mappings), std::move (vis), std::move (outer_attrs)),
    qualifiers (std::move (qualifiers)),
    function_name (std::move (function_name)),
    generic_params (std::move (generic_params)),
    function_params (std::move (function_params)),
    return_type (std::move (return_type)),
    where_clause (std::move (where_clause)),
    function_body (std::move (function_body)), self (std::move (self)),
    locus (locus), defaultness (defaultness)
{}

Function::Function (Function const &other)
  : VisItem (other), qualifiers (other.qualifiers),
    function_name (other.function_name),
    function_params (other.function_params), where_clause (other.where_clause),
    function_body (other.function_body->clone_block_expr ()), self (other.self),
    locus (other.locus), defaultness (other.defaultness)
{
  // guard to prevent null dereference (always required)
  if (other.return_type != nullptr)
    return_type = other.return_type->clone_type ();
  else
    return_type = nullptr;

  generic_params.reserve (other.generic_params.size ());
  for (const auto &e : other.generic_params)
    generic_params.push_back (e->clone_generic_param ());
}

Function &
Function::operator= (Function const &other)
{
  VisItem::operator= (other);
  function_name = other.function_name;
  qualifiers = other.qualifiers;
  function_params = other.function_params;

  // guard to prevent null dereference (always required)
  if (other.return_type != nullptr)
    return_type = other.return_type->clone_type ();
  else
    return_type = nullptr;

  where_clause = other.where_clause;
  function_body = other.function_body->clone_block_expr ();
  locus = other.locus;
  self = other.self;

  defaultness = other.defaultness;

  generic_params.reserve (other.generic_params.size ());
  for (const auto &e : other.generic_params)
    generic_params.push_back (e->clone_generic_param ());

  return *this;
}

TypeAlias::TypeAlias (Analysis::NodeMapping mappings, Identifier new_type_name,
		      std::vector<std::unique_ptr<GenericParam>> generic_params,
		      WhereClause where_clause,
		      std::unique_ptr<Type> existing_type, Visibility vis,
		      AST::AttrVec outer_attrs, location_t locus)
  : VisItem (std::move (mappings), std::move (vis), std::move (outer_attrs)),
    new_type_name (std::move (new_type_name)),
    generic_params (std::move (generic_params)),
    where_clause (std::move (where_clause)),
    existing_type (std::move (existing_type)), locus (locus)
{}

TypeAlias::TypeAlias (TypeAlias const &other)
  : VisItem (other), new_type_name (other.new_type_name),
    where_clause (other.where_clause),
    existing_type (other.existing_type->clone_type ()), locus (other.locus)
{
  generic_params.reserve (other.generic_params.size ());
  for (const auto &e : other.generic_params)
    generic_params.push_back (e->clone_generic_param ());
}

TypeAlias &
TypeAlias::operator= (TypeAlias const &other)
{
  VisItem::operator= (other);
  new_type_name = other.new_type_name;
  where_clause = other.where_clause;
  existing_type = other.existing_type->clone_type ();
  locus = other.locus;

  generic_params.reserve (other.generic_params.size ());
  for (const auto &e : other.generic_params)
    generic_params.push_back (e->clone_generic_param ());

  return *this;
}

StructField::StructField (Analysis::NodeMapping mappings, Identifier field_name,
			  std::unique_ptr<Type> field_type, Visibility vis,
			  location_t locus, AST::AttrVec outer_attrs)
  : outer_attrs (std::move (outer_attrs)), visibility (std::move (vis)),
    field_name (std::move (field_name)), field_type (std::move (field_type)),
    mappings (mappings), locus (locus)
{}

StructField::StructField (StructField const &other)
  : outer_attrs (other.outer_attrs), visibility (other.visibility),
    field_name (other.field_name), field_type (other.field_type->clone_type ()),
    mappings (other.mappings)
{}

StructField &
StructField::operator= (StructField const &other)
{
  field_name = other.field_name;
  field_type = other.field_type->clone_type ();
  visibility = other.visibility;
  outer_attrs = other.outer_attrs;
  mappings = other.mappings;

  return *this;
}

TupleField::TupleField (Analysis::NodeMapping mapping,
			std::unique_ptr<Type> field_type, Visibility vis,
			location_t locus, AST::AttrVec outer_attrs)
  : outer_attrs (std::move (outer_attrs)), visibility (std::move (vis)),
    field_type (std::move (field_type)), locus (locus), mappings (mapping)
{}

TupleField::TupleField (TupleField const &other)
  : outer_attrs (other.outer_attrs), visibility (other.visibility),
    field_type (other.field_type->clone_type ()), locus (other.locus),
    mappings (other.mappings)
{}

TupleField &
TupleField::operator= (TupleField const &other)
{
  field_type = other.field_type->clone_type ();
  visibility = other.visibility;
  outer_attrs = other.outer_attrs;
  locus = other.locus;
  mappings = other.mappings;

  return *this;
}

TupleStruct::TupleStruct (
  Analysis::NodeMapping mappings, std::vector<TupleField> fields,
  Identifier struct_name,
  std::vector<std::unique_ptr<GenericParam>> generic_params,
  WhereClause where_clause, Visibility vis, AST::AttrVec outer_attrs,
  location_t locus)
  : Struct (std::move (mappings), std::move (struct_name),
	    std::move (generic_params), std::move (where_clause),
	    std::move (vis), locus, std::move (outer_attrs)),
    fields (std::move (fields))
{}

EnumItem::EnumItem (Analysis::NodeMapping mappings, Identifier variant_name,
		    AST::AttrVec outer_attrs, location_t locus)
  : Item (std::move (mappings), std::move (outer_attrs)),
    variant_name (std::move (variant_name)), locus (locus)
{}

EnumItemTuple::EnumItemTuple (Analysis::NodeMapping mappings,
			      Identifier variant_name,
			      std::vector<TupleField> tuple_fields,
			      AST::AttrVec outer_attrs, location_t locus)
  : EnumItem (std::move (mappings), std::move (variant_name),
	      std::move (outer_attrs), locus),
    tuple_fields (std::move (tuple_fields))
{}

EnumItemStruct::EnumItemStruct (Analysis::NodeMapping mappings,
				Identifier variant_name,
				std::vector<StructField> struct_fields,
				AST::AttrVec outer_attrs, location_t locus)
  : EnumItem (std::move (mappings), std::move (variant_name),
	      std::move (outer_attrs), locus),
    struct_fields (std::move (struct_fields))
{}

EnumItemDiscriminant::EnumItemDiscriminant (Analysis::NodeMapping mappings,
					    Identifier variant_name,
					    std::unique_ptr<Expr> expr,
					    AST::AttrVec outer_attrs,
					    location_t locus)
  : EnumItem (std::move (mappings), std::move (variant_name),
	      std::move (outer_attrs), locus),
    expression (std::move (expr))
{}

EnumItemDiscriminant::EnumItemDiscriminant (EnumItemDiscriminant const &other)
  : EnumItem (other), expression (other.expression->clone_expr ())
{}

EnumItemDiscriminant &
EnumItemDiscriminant::operator= (EnumItemDiscriminant const &other)
{
  EnumItem::operator= (other);
  expression = other.expression->clone_expr ();
  // variant_name = other.variant_name;
  // outer_attrs = other.outer_attrs;

  return *this;
}

Enum::Enum (Analysis::NodeMapping mappings, Identifier enum_name,
	    Visibility vis,
	    std::vector<std::unique_ptr<GenericParam>> generic_params,
	    WhereClause where_clause,
	    std::vector<std::unique_ptr<EnumItem>> items,
	    AST::AttrVec outer_attrs, location_t locus)
  : VisItem (std::move (mappings), std::move (vis), std::move (outer_attrs)),
    enum_name (std::move (enum_name)),
    generic_params (std::move (generic_params)),
    where_clause (std::move (where_clause)), items (std::move (items)),
    locus (locus)
{}

Enum::Enum (Enum const &other)
  : VisItem (other), enum_name (other.enum_name),
    where_clause (other.where_clause), locus (other.locus)
{
  generic_params.reserve (other.generic_params.size ());
  for (const auto &e : other.generic_params)
    generic_params.push_back (e->clone_generic_param ());

  items.reserve (other.items.size ());
  for (const auto &e : other.items)
    items.push_back (e->clone_enum_item ());
}

Enum &
Enum::operator= (Enum const &other)
{
  VisItem::operator= (other);
  enum_name = other.enum_name;
  where_clause = other.where_clause;
  locus = other.locus;

  generic_params.reserve (other.generic_params.size ());
  for (const auto &e : other.generic_params)
    generic_params.push_back (e->clone_generic_param ());

  items.reserve (other.items.size ());
  for (const auto &e : other.items)
    items.push_back (e->clone_enum_item ());

  return *this;
}

Union::Union (Analysis::NodeMapping mappings, Identifier union_name,
	      Visibility vis,
	      std::vector<std::unique_ptr<GenericParam>> generic_params,
	      WhereClause where_clause, std::vector<StructField> variants,
	      AST::AttrVec outer_attrs, location_t locus)
  : VisItem (std::move (mappings), std::move (vis), std::move (outer_attrs)),
    union_name (std::move (union_name)),
    generic_params (std::move (generic_params)),
    where_clause (std::move (where_clause)), variants (std::move (variants)),
    locus (locus)
{}

Union::Union (Union const &other)
  : VisItem (other), union_name (other.union_name),
    where_clause (other.where_clause), variants (other.variants),
    locus (other.locus)
{
  generic_params.reserve (other.generic_params.size ());
  for (const auto &e : other.generic_params)
    generic_params.push_back (e->clone_generic_param ());
}

Union &
Union::operator= (Union const &other)
{
  VisItem::operator= (other);
  union_name = other.union_name;
  where_clause = other.where_clause;
  variants = other.variants;
  locus = other.locus;

  generic_params.reserve (other.generic_params.size ());
  for (const auto &e : other.generic_params)
    generic_params.push_back (e->clone_generic_param ());

  return *this;
}

ConstantItem::ConstantItem (Analysis::NodeMapping mappings, Identifier ident,
			    Visibility vis, std::unique_ptr<Type> type,
			    std::unique_ptr<Expr> const_expr,
			    AST::AttrVec outer_attrs, location_t locus)
  : VisItem (std::move (mappings), std::move (vis), std::move (outer_attrs)),
    identifier (std::move (ident)), type (std::move (type)),
    const_expr (std::move (const_expr)), locus (locus)
{}

ConstantItem::ConstantItem (ConstantItem const &other)
  : VisItem (other), identifier (other.identifier),
    type (other.type->clone_type ()),
    const_expr (other.const_expr->clone_expr ()), locus (other.locus)
{}

ConstantItem &
ConstantItem::operator= (ConstantItem const &other)
{
  VisItem::operator= (other);
  identifier = other.identifier;
  type = other.type->clone_type ();
  const_expr = other.const_expr->clone_expr ();
  locus = other.locus;

  return *this;
}

StaticItem::StaticItem (Analysis::NodeMapping mappings, Identifier name,
			Mutability mut, std::unique_ptr<Type> type,
			std::unique_ptr<Expr> expr, Visibility vis,
			AST::AttrVec outer_attrs, location_t locus)
  : VisItem (std::move (mappings), std::move (vis), std::move (outer_attrs)),
    mut (mut), name (std::move (name)), type (std::move (type)),
    expr (std::move (expr)), locus (locus)
{}

StaticItem::StaticItem (StaticItem const &other)
  : VisItem (other), mut (other.mut), name (other.name),
    type (other.type->clone_type ()), expr (other.expr->clone_expr ()),
    locus (other.locus)
{}

StaticItem &
StaticItem::operator= (StaticItem const &other)
{
  VisItem::operator= (other);
  name = other.name;
  mut = other.mut;
  type = other.type->clone_type ();
  expr = other.expr->clone_expr ();
  locus = other.locus;

  return *this;
}

TraitFunctionDecl::TraitFunctionDecl (
  Identifier function_name, FunctionQualifiers qualifiers,
  std::vector<std::unique_ptr<GenericParam>> generic_params,
  tl::optional<SelfParam> self, std::vector<FunctionParam> function_params,
  std::unique_ptr<Type> return_type, WhereClause where_clause)
  : qualifiers (std::move (qualifiers)),
    function_name (std::move (function_name)),
    generic_params (std::move (generic_params)),
    function_params (std::move (function_params)),
    return_type (std::move (return_type)),
    where_clause (std::move (where_clause)), self (std::move (self))
{}

TraitFunctionDecl::TraitFunctionDecl (TraitFunctionDecl const &other)
  : qualifiers (other.qualifiers), function_name (other.function_name),
    function_params (other.function_params),
    return_type (other.return_type->clone_type ()),
    where_clause (other.where_clause), self (other.self)
{
  generic_params.reserve (other.generic_params.size ());
  for (const auto &e : other.generic_params)
    generic_params.push_back (e->clone_generic_param ());
}

TraitFunctionDecl &
TraitFunctionDecl::operator= (TraitFunctionDecl const &other)
{
  function_name = other.function_name;
  qualifiers = other.qualifiers;
  function_params = other.function_params;
  return_type = other.return_type->clone_type ();
  where_clause = other.where_clause;
  self = other.self;

  generic_params.reserve (other.generic_params.size ());
  for (const auto &e : other.generic_params)
    generic_params.push_back (e->clone_generic_param ());

  return *this;
}

TraitItemFunc::TraitItemFunc (Analysis::NodeMapping mappings,
			      TraitFunctionDecl decl,
			      std::unique_ptr<BlockExpr> block_expr,
			      AST::AttrVec outer_attrs, location_t locus)
  : TraitItem (mappings), outer_attrs (std::move (outer_attrs)),
    decl (std::move (decl)), block_expr (std::move (block_expr)), locus (locus)
{}

TraitItemFunc::TraitItemFunc (TraitItemFunc const &other)
  : TraitItem (other.mappings), outer_attrs (other.outer_attrs),
    decl (other.decl), locus (other.locus)
{
  if (other.block_expr != nullptr)
    block_expr = other.block_expr->clone_block_expr ();
}

TraitItemFunc &
TraitItemFunc::operator= (TraitItemFunc const &other)
{
  TraitItem::operator= (other);
  outer_attrs = other.outer_attrs;
  decl = other.decl;
  locus = other.locus;
  mappings = other.mappings;
  if (other.block_expr != nullptr)
    block_expr = other.block_expr->clone_block_expr ();

  return *this;
}

TraitItemConst::TraitItemConst (Analysis::NodeMapping mappings, Identifier name,
				std::unique_ptr<Type> type,
				std::unique_ptr<Expr> expr,
				AST::AttrVec outer_attrs, location_t locus)
  : TraitItem (mappings), outer_attrs (std::move (outer_attrs)),
    name (std::move (name)), type (std::move (type)), expr (std::move (expr)),
    locus (locus)
{}

TraitItemConst::TraitItemConst (TraitItemConst const &other)
  : TraitItem (other.mappings), outer_attrs (other.outer_attrs),
    name (other.name), type (other.type->clone_type ()),
    expr (other.expr->clone_expr ()), locus (other.locus)
{}

TraitItemConst &
TraitItemConst::operator= (TraitItemConst const &other)
{
  TraitItem::operator= (other);
  outer_attrs = other.outer_attrs;
  name = other.name;
  type = other.type->clone_type ();
  expr = other.expr->clone_expr ();
  locus = other.locus;
  mappings = other.mappings;

  return *this;
}

TraitItemType::TraitItemType (
  Analysis::NodeMapping mappings, Identifier name,
  std::vector<std::unique_ptr<TypeParamBound>> type_param_bounds,
  AST::AttrVec outer_attrs, location_t locus)
  : TraitItem (mappings), outer_attrs (std::move (outer_attrs)),
    name (std::move (name)), type_param_bounds (std::move (type_param_bounds)),
    locus (locus)
{}

TraitItemType::TraitItemType (TraitItemType const &other)
  : TraitItem (other.mappings), outer_attrs (other.outer_attrs),
    name (other.name), locus (other.locus)
{
  type_param_bounds.reserve (other.type_param_bounds.size ());
  for (const auto &e : other.type_param_bounds)
    type_param_bounds.push_back (e->clone_type_param_bound ());
}

TraitItemType &
TraitItemType::operator= (TraitItemType const &other)
{
  TraitItem::operator= (other);
  outer_attrs = other.outer_attrs;
  name = other.name;
  locus = other.locus;
  mappings = other.mappings;

  type_param_bounds.reserve (other.type_param_bounds.size ());
  for (const auto &e : other.type_param_bounds)
    type_param_bounds.push_back (e->clone_type_param_bound ());

  return *this;
}

Trait::Trait (Analysis::NodeMapping mappings, Identifier name,
	      Unsafety unsafety,
	      std::vector<std::unique_ptr<GenericParam>> generic_params,
	      std::vector<std::unique_ptr<TypeParamBound>> type_param_bounds,
	      WhereClause where_clause,
	      std::vector<std::unique_ptr<TraitItem>> trait_items,
	      Visibility vis, AST::AttrVec outer_attrs, location_t locus)
  : VisItem (std::move (mappings), std::move (vis), std::move (outer_attrs)),
    unsafety (unsafety), name (std::move (name)),
    generic_params (std::move (generic_params)),
    type_param_bounds (std::move (type_param_bounds)),
    where_clause (std::move (where_clause)),
    trait_items (std::move (trait_items)), locus (locus)
{}

Trait::Trait (Trait const &other)
  : VisItem (other), unsafety (other.unsafety), name (other.name),
    where_clause (other.where_clause), locus (other.locus)
{
  generic_params.reserve (other.generic_params.size ());
  for (const auto &e : other.generic_params)
    generic_params.push_back (e->clone_generic_param ());

  type_param_bounds.reserve (other.type_param_bounds.size ());
  for (const auto &e : other.type_param_bounds)
    type_param_bounds.push_back (e->clone_type_param_bound ());

  trait_items.reserve (other.trait_items.size ());
  for (const auto &e : other.trait_items)
    trait_items.push_back (e->clone_trait_item ());
}

Trait &
Trait::operator= (Trait const &other)
{
  VisItem::operator= (other);
  name = other.name;
  unsafety = other.unsafety;
  where_clause = other.where_clause;
  locus = other.locus;

  generic_params.reserve (other.generic_params.size ());
  for (const auto &e : other.generic_params)
    generic_params.push_back (e->clone_generic_param ());

  type_param_bounds.reserve (other.type_param_bounds.size ());
  for (const auto &e : other.type_param_bounds)
    type_param_bounds.push_back (e->clone_type_param_bound ());

  trait_items.reserve (other.trait_items.size ());
  for (const auto &e : other.trait_items)
    trait_items.push_back (e->clone_trait_item ());

  return *this;
}

ImplBlock::ImplBlock (Analysis::NodeMapping mappings,
		      std::vector<std::unique_ptr<ImplItem>> impl_items,
		      std::vector<std::unique_ptr<GenericParam>> generic_params,
		      std::unique_ptr<Type> impl_type,
		      std::unique_ptr<TypePath> trait_ref,
		      WhereClause where_clause, BoundPolarity polarity,
		      Visibility vis, AST::AttrVec inner_attrs,
		      AST::AttrVec outer_attrs, location_t locus, bool unsafe)
  : VisItem (std::move (mappings), std::move (vis), std::move (outer_attrs)),
    WithInnerAttrs (std::move (inner_attrs)),
    generic_params (std::move (generic_params)),
    impl_type (std::move (impl_type)), trait_ref (std::move (trait_ref)),
    where_clause (std::move (where_clause)), polarity (polarity), locus (locus),
    impl_items (std::move (impl_items)), unsafe (unsafe)
{}

ImplBlock::ImplBlock (ImplBlock const &other)
  : VisItem (other), WithInnerAttrs (other.inner_attrs),
    impl_type (other.impl_type->clone_type ()),
    where_clause (other.where_clause), polarity (other.polarity),
    locus (other.locus), unsafe (other.unsafe)
{
  generic_params.reserve (other.generic_params.size ());
  for (const auto &e : other.generic_params)
    generic_params.push_back (e->clone_generic_param ());

  impl_items.reserve (other.impl_items.size ());
  for (const auto &e : other.impl_items)
    impl_items.push_back (e->clone_inherent_impl_item ());
}

ImplBlock &
ImplBlock::operator= (ImplBlock const &other)
{
  VisItem::operator= (other);
  impl_type = other.impl_type->clone_type ();
  where_clause = other.where_clause;
  polarity = other.polarity;
  inner_attrs = other.inner_attrs;
  locus = other.locus;
  unsafe = other.unsafe;

  generic_params.reserve (other.generic_params.size ());
  for (const auto &e : other.generic_params)
    generic_params.push_back (e->clone_generic_param ());

  impl_items.reserve (other.impl_items.size ());
  for (const auto &e : other.impl_items)
    impl_items.push_back (e->clone_inherent_impl_item ());

  return *this;
}

ExternalItem::ExternalItem (Analysis::NodeMapping mappings,
			    Identifier item_name, Visibility vis,
			    AST::AttrVec outer_attrs, location_t locus)
  : mappings (mappings), outer_attrs (std::move (outer_attrs)),
    visibility (std::move (vis)), item_name (std::move (item_name)),
    locus (locus)
{}

ExternalItem::ExternalItem (ExternalItem const &other)
  : mappings (other.mappings), outer_attrs (other.outer_attrs),
    visibility (other.visibility), item_name (other.item_name),
    locus (other.locus)
{}

ExternalItem &
ExternalItem::operator= (ExternalItem const &other)
{
  mappings = other.mappings;
  item_name = other.item_name;
  visibility = other.visibility;
  outer_attrs = other.outer_attrs;
  locus = other.locus;

  return *this;
}

ExternalStaticItem::ExternalStaticItem (Analysis::NodeMapping mappings,
					Identifier item_name,
					std::unique_ptr<Type> item_type,
					Mutability mut, Visibility vis,
					AST::AttrVec outer_attrs,
					location_t locus)
  : ExternalItem (std::move (mappings), std::move (item_name), std::move (vis),
		  std::move (outer_attrs), locus),
    mut (mut), item_type (std::move (item_type))
{}

ExternalStaticItem::ExternalStaticItem (ExternalStaticItem const &other)
  : ExternalItem (other), mut (other.mut),
    item_type (other.item_type->clone_type ())
{}

ExternalStaticItem &
ExternalStaticItem::operator= (ExternalStaticItem const &other)
{
  ExternalItem::operator= (other);
  item_type = other.item_type->clone_type ();
  mut = other.mut;

  return *this;
}

NamedFunctionParam::NamedFunctionParam (Analysis::NodeMapping mappings,
					Identifier name,
					std::unique_ptr<Type> param_type)
  : name (std::move (name)), param_type (std::move (param_type)),
    mappings (std::move (mappings))
{}

NamedFunctionParam::NamedFunctionParam (NamedFunctionParam const &other)
  : name (other.name), param_type (other.param_type->clone_type ()),
    mappings (other.mappings)
{}

NamedFunctionParam &
NamedFunctionParam::operator= (NamedFunctionParam const &other)
{
  mappings = other.mappings;
  name = other.name;
  param_type = other.param_type->clone_type ();
  // has_name = other.has_name;

  return *this;
}

ExternalFunctionItem::ExternalFunctionItem (
  Analysis::NodeMapping mappings, Identifier item_name,
  std::vector<std::unique_ptr<GenericParam>> generic_params,
  std::unique_ptr<Type> return_type, WhereClause where_clause,
  std::vector<NamedFunctionParam> function_params, bool has_variadics,
  Visibility vis, AST::AttrVec outer_attrs, location_t locus)
  : ExternalItem (std::move (mappings), std::move (item_name), std::move (vis),
		  std::move (outer_attrs), locus),
    generic_params (std::move (generic_params)),
    return_type (std::move (return_type)),
    where_clause (std::move (where_clause)),
    function_params (std::move (function_params)), has_variadics (has_variadics)
{}

ExternalFunctionItem::ExternalFunctionItem (ExternalFunctionItem const &other)
  : ExternalItem (other), where_clause (other.where_clause),
    function_params (other.function_params), has_variadics (other.has_variadics)
{
  if (other.return_type)
    return_type = other.return_type->clone_type ();

  generic_params.reserve (other.generic_params.size ());
  for (const auto &e : other.generic_params)
    generic_params.push_back (e->clone_generic_param ());
}

ExternalFunctionItem &
ExternalFunctionItem::operator= (ExternalFunctionItem const &other)
{
  ExternalItem::operator= (other);

  where_clause = other.where_clause;
  function_params = other.function_params;
  has_variadics = other.has_variadics;

  if (other.return_type)
    return_type = other.return_type->clone_type ();

  generic_params.reserve (other.generic_params.size ());
  for (const auto &e : other.generic_params)
    generic_params.push_back (e->clone_generic_param ());

  return *this;
}

ExternalTypeItem::ExternalTypeItem (Analysis::NodeMapping mappings,
				    Identifier item_name, Visibility vis,
				    location_t locus)
  : ExternalItem (std::move (mappings), std::move (item_name),
		  Visibility (std::move (vis)),
		  /* FIXME: Is that correct? */
		  {}, locus)
{}

ExternalTypeItem::ExternalTypeItem (ExternalTypeItem const &other)
  : ExternalItem (other)
{}

ExternBlock::ExternBlock (
  Analysis::NodeMapping mappings, ABI abi,
  std::vector<std::unique_ptr<ExternalItem>> extern_items, Visibility vis,
  AST::AttrVec inner_attrs, AST::AttrVec outer_attrs, location_t locus)
  : VisItem (std::move (mappings), std::move (vis), std::move (outer_attrs)),
    WithInnerAttrs (std::move (inner_attrs)), abi (abi),
    extern_items (std::move (extern_items)), locus (locus)
{}

ExternBlock::ExternBlock (ExternBlock const &other)
  : VisItem (other), WithInnerAttrs (other.inner_attrs), abi (other.abi),
    locus (other.locus)
{
  extern_items.reserve (other.extern_items.size ());
  for (const auto &e : other.extern_items)
    extern_items.push_back (e->clone_external_item ());
}

ExternBlock &
ExternBlock::operator= (ExternBlock const &other)
{
  VisItem::operator= (other);
  abi = other.abi;
  inner_attrs = other.inner_attrs;
  locus = other.locus;

  extern_items.reserve (other.extern_items.size ());
  for (const auto &e : other.extern_items)
    extern_items.push_back (e->clone_external_item ());

  return *this;
}

} // namespace HIR
} // namespace Rust
