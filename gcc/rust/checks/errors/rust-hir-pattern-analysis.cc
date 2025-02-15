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

#include "rust-system.h"
#include "rust-hir-pattern-analysis.h"
#include "rust-diagnostics.h"
#include "rust-hir-full-decls.h"
#include "rust-hir-path.h"
#include "rust-hir-pattern.h"
#include "rust-hir.h"
#include "rust-mapping-common.h"
#include "rust-system.h"
#include "rust-tyty.h"
#include "rust-immutable-name-resolution-context.h"

// for flag_name_resolution_2_0
#include "options.h"

namespace Rust {
namespace Analysis {

PatternChecker::PatternChecker ()
  : tyctx (*Resolver::TypeCheckContext::get ()),
    resolver (*Resolver::Resolver::get ()),
    mappings (Analysis::Mappings::get ())
{}

void
PatternChecker::go (HIR::Crate &crate)
{
  rust_debug ("started pattern check");
  for (auto &item : crate.get_items ())
    item->accept_vis (*this);
  rust_debug ("finished pattern check");
}

void
PatternChecker::visit (Lifetime &)
{}

void
PatternChecker::visit (LifetimeParam &)
{}

void
PatternChecker::visit (PathInExpression &path)
{}

void
PatternChecker::visit (TypePathSegment &)
{}

void
PatternChecker::visit (TypePathSegmentGeneric &)
{}

void
PatternChecker::visit (TypePathSegmentFunction &)
{}

void
PatternChecker::visit (TypePath &)
{}

void
PatternChecker::visit (QualifiedPathInExpression &)
{}

void
PatternChecker::visit (QualifiedPathInType &)
{}

void
PatternChecker::visit (LiteralExpr &)
{}

void
PatternChecker::visit (BorrowExpr &expr)
{
  expr.get_expr ().accept_vis (*this);
}

void
PatternChecker::visit (DereferenceExpr &expr)
{
  expr.get_expr ().accept_vis (*this);
}

void
PatternChecker::visit (ErrorPropagationExpr &expr)
{
  expr.get_expr ().accept_vis (*this);
}

void
PatternChecker::visit (NegationExpr &expr)
{
  expr.get_expr ().accept_vis (*this);
}

void
PatternChecker::visit (ArithmeticOrLogicalExpr &expr)
{
  expr.get_lhs ().accept_vis (*this);
  expr.get_rhs ().accept_vis (*this);
}

void
PatternChecker::visit (ComparisonExpr &expr)
{
  expr.get_lhs ().accept_vis (*this);
  expr.get_rhs ().accept_vis (*this);
}

void
PatternChecker::visit (LazyBooleanExpr &expr)
{
  expr.get_lhs ().accept_vis (*this);
  expr.get_rhs ().accept_vis (*this);
}

void
PatternChecker::visit (TypeCastExpr &expr)
{
  expr.get_expr ().accept_vis (*this);
}

void
PatternChecker::visit (AssignmentExpr &expr)
{
  expr.get_lhs ().accept_vis (*this);
  expr.get_rhs ().accept_vis (*this);
}

void
PatternChecker::visit (CompoundAssignmentExpr &expr)
{
  expr.get_lhs ().accept_vis (*this);
  expr.get_rhs ().accept_vis (*this);
}

void
PatternChecker::visit (GroupedExpr &expr)
{
  expr.get_expr_in_parens ().accept_vis (*this);
}

void
PatternChecker::visit (ArrayElemsValues &elems)
{
  for (auto &elem : elems.get_values ())
    elem->accept_vis (*this);
}

void
PatternChecker::visit (ArrayElemsCopied &elems)
{
  elems.get_elem_to_copy ().accept_vis (*this);
}

void
PatternChecker::visit (ArrayExpr &expr)
{
  expr.get_internal_elements ().accept_vis (*this);
}

void
PatternChecker::visit (ArrayIndexExpr &expr)
{
  expr.get_array_expr ().accept_vis (*this);
  expr.get_index_expr ().accept_vis (*this);
}

void
PatternChecker::visit (TupleExpr &expr)
{
  for (auto &elem : expr.get_tuple_elems ())
    elem->accept_vis (*this);
}

void
PatternChecker::visit (TupleIndexExpr &expr)
{
  expr.get_tuple_expr ().accept_vis (*this);
}

void
PatternChecker::visit (StructExprStruct &)
{}

void
PatternChecker::visit (StructExprFieldIdentifier &)
{}

void
PatternChecker::visit (StructExprFieldIdentifierValue &field)
{
  field.get_value ().accept_vis (*this);
}

void
PatternChecker::visit (StructExprFieldIndexValue &field)
{
  field.get_value ().accept_vis (*this);
}

void
PatternChecker::visit (StructExprStructFields &expr)
{
  for (auto &field : expr.get_fields ())
    field->accept_vis (*this);
}

void
PatternChecker::visit (StructExprStructBase &)
{}

void
PatternChecker::visit (CallExpr &expr)
{
  if (!expr.has_fnexpr ())
    return;

  NodeId ast_node_id = expr.get_fnexpr ().get_mappings ().get_nodeid ();
  NodeId ref_node_id;
  if (flag_name_resolution_2_0)
    {
      auto &nr_ctx
	= Resolver2_0::ImmutableNameResolutionContext::get ().resolver ();

      if (auto id = nr_ctx.lookup (ast_node_id))
	ref_node_id = *id;
      else
	return;
    }
  else if (!resolver.lookup_resolved_name (ast_node_id, &ref_node_id))
    return;

  if (auto definition_id = mappings.lookup_node_to_hir (ref_node_id))
    {
      if (expr.has_params ())
	for (auto &arg : expr.get_arguments ())
	  arg->accept_vis (*this);
    }
  else
    {
      rust_unreachable ();
    }
}

void
PatternChecker::visit (MethodCallExpr &expr)
{
  expr.get_receiver ().accept_vis (*this);

  for (auto &arg : expr.get_arguments ())
    arg->accept_vis (*this);
}

void
PatternChecker::visit (FieldAccessExpr &expr)
{
  expr.get_receiver_expr ().accept_vis (*this);
}

void
PatternChecker::visit (ClosureExpr &expr)
{
  expr.get_expr ().accept_vis (*this);
}

void
PatternChecker::visit (BlockExpr &expr)
{
  for (auto &stmt : expr.get_statements ())
    stmt->accept_vis (*this);

  if (expr.has_expr ())
    expr.get_final_expr ().accept_vis (*this);
}

void
PatternChecker::visit (ContinueExpr &)
{}

void
PatternChecker::visit (BreakExpr &expr)
{
  if (expr.has_break_expr ())
    expr.get_expr ().accept_vis (*this);
}

void
PatternChecker::visit (RangeFromToExpr &expr)
{
  expr.get_from_expr ().accept_vis (*this);
  expr.get_to_expr ().accept_vis (*this);
}

void
PatternChecker::visit (RangeFromExpr &expr)
{
  expr.get_from_expr ().accept_vis (*this);
}

void
PatternChecker::visit (RangeToExpr &expr)
{
  expr.get_to_expr ().accept_vis (*this);
}

void
PatternChecker::visit (RangeFullExpr &)
{}

void
PatternChecker::visit (RangeFromToInclExpr &expr)
{
  expr.get_from_expr ().accept_vis (*this);
  expr.get_to_expr ().accept_vis (*this);
}

void
PatternChecker::visit (RangeToInclExpr &expr)
{
  expr.get_to_expr ().accept_vis (*this);
}

void
PatternChecker::visit (ReturnExpr &expr)
{
  if (expr.has_return_expr ())
    expr.get_expr ().accept_vis (*this);
}

void
PatternChecker::visit (UnsafeBlockExpr &expr)
{
  expr.get_block_expr ().accept_vis (*this);
}

void
PatternChecker::visit (LoopExpr &expr)
{
  expr.get_loop_block ().accept_vis (*this);
}

void
PatternChecker::visit (WhileLoopExpr &expr)
{
  expr.get_predicate_expr ().accept_vis (*this);
  expr.get_loop_block ().accept_vis (*this);
}

void
PatternChecker::visit (WhileLetLoopExpr &expr)
{
  expr.get_cond ().accept_vis (*this);
  expr.get_loop_block ().accept_vis (*this);
}

void
PatternChecker::visit (IfExpr &expr)
{
  expr.get_if_condition ().accept_vis (*this);
  expr.get_if_block ().accept_vis (*this);
}

void
PatternChecker::visit (IfExprConseqElse &expr)
{
  expr.get_if_condition ().accept_vis (*this);
  expr.get_if_block ().accept_vis (*this);
  expr.get_else_block ().accept_vis (*this);
}

void
PatternChecker::visit (MatchExpr &expr)
{
  expr.get_scrutinee_expr ().accept_vis (*this);

  for (auto &match_arm : expr.get_match_cases ())
    match_arm.get_expr ().accept_vis (*this);

  // match expressions are only an entrypoint
  TyTy::BaseType *scrutinee_ty;
  bool ok = tyctx.lookup_type (
    expr.get_scrutinee_expr ().get_mappings ().get_hirid (), &scrutinee_ty);
  rust_assert (ok);

  check_match_usefulness (&tyctx, scrutinee_ty, expr);
}

void
PatternChecker::visit (AwaitExpr &)
{
  // TODO: Visit expression
}

void
PatternChecker::visit (AsyncBlockExpr &)
{
  // TODO: Visit block expression
}

void
PatternChecker::visit (InlineAsm &expr)
{}

void
PatternChecker::visit (TypeParam &)
{}

void
PatternChecker::visit (ConstGenericParam &)
{}

void
PatternChecker::visit (LifetimeWhereClauseItem &)
{}

void
PatternChecker::visit (TypeBoundWhereClauseItem &)
{}

void
PatternChecker::visit (Module &module)
{
  for (auto &item : module.get_items ())
    item->accept_vis (*this);
}

void
PatternChecker::visit (ExternCrate &)
{}

void
PatternChecker::visit (UseTreeGlob &)
{}

void
PatternChecker::visit (UseTreeList &)
{}

void
PatternChecker::visit (UseTreeRebind &)
{}

void
PatternChecker::visit (UseDeclaration &)
{}

void
PatternChecker::visit (Function &function)
{
  function.get_definition ().accept_vis (*this);
}

void
PatternChecker::visit (TypeAlias &)
{}

void
PatternChecker::visit (StructStruct &)
{}

void
PatternChecker::visit (TupleStruct &)
{}

void
PatternChecker::visit (EnumItem &)
{}

void
PatternChecker::visit (EnumItemTuple &)
{}

void
PatternChecker::visit (EnumItemStruct &)
{}

void
PatternChecker::visit (EnumItemDiscriminant &)
{}

void
PatternChecker::visit (Enum &)
{}

void
PatternChecker::visit (Union &)
{}

void
PatternChecker::visit (ConstantItem &const_item)
{
  const_item.get_expr ().accept_vis (*this);
}

void
PatternChecker::visit (StaticItem &static_item)
{
  static_item.get_expr ().accept_vis (*this);
}

void
PatternChecker::visit (TraitItemFunc &item)
{
  if (item.has_definition ())
    item.get_block_expr ().accept_vis (*this);
}

void
PatternChecker::visit (TraitItemConst &item)
{
  if (item.has_expr ())
    item.get_expr ().accept_vis (*this);
}

void
PatternChecker::visit (TraitItemType &)
{}

void
PatternChecker::visit (Trait &trait)
{
  for (auto &item : trait.get_trait_items ())
    item->accept_vis (*this);
}

void
PatternChecker::visit (ImplBlock &impl)
{
  for (auto &item : impl.get_impl_items ())
    item->accept_vis (*this);
}

void
PatternChecker::visit (ExternalStaticItem &)
{}

void
PatternChecker::visit (ExternalFunctionItem &)
{}

void
PatternChecker::visit (ExternalTypeItem &)
{}

void
PatternChecker::visit (ExternBlock &block)
{
  // FIXME: Do we need to do this?
  for (auto &item : block.get_extern_items ())
    item->accept_vis (*this);
}

void
PatternChecker::visit (LiteralPattern &)
{}

void
PatternChecker::visit (IdentifierPattern &)
{}

void
PatternChecker::visit (WildcardPattern &)
{}

void
PatternChecker::visit (RangePatternBoundLiteral &)
{}

void
PatternChecker::visit (RangePatternBoundPath &)
{}

void
PatternChecker::visit (RangePatternBoundQualPath &)
{}

void
PatternChecker::visit (RangePattern &)
{}

void
PatternChecker::visit (ReferencePattern &)
{}

void
PatternChecker::visit (StructPatternFieldTuplePat &)
{}

void
PatternChecker::visit (StructPatternFieldIdentPat &)
{}

void
PatternChecker::visit (StructPatternFieldIdent &)
{}

void
PatternChecker::visit (StructPattern &)
{}

void
PatternChecker::visit (TupleStructItemsNoRange &)
{}

void
PatternChecker::visit (TupleStructItemsRange &)
{}

void
PatternChecker::visit (TupleStructPattern &)
{}

void
PatternChecker::visit (TuplePatternItemsMultiple &)
{}

void
PatternChecker::visit (TuplePatternItemsRanged &)
{}

void
PatternChecker::visit (TuplePattern &)
{}

void
PatternChecker::visit (SlicePattern &)
{}

void
PatternChecker::visit (AltPattern &)
{}

void
PatternChecker::visit (EmptyStmt &)
{}

void
PatternChecker::visit (LetStmt &stmt)
{
  if (stmt.has_init_expr ())
    stmt.get_init_expr ().accept_vis (*this);
}

void
PatternChecker::visit (ExprStmt &stmt)
{
  stmt.get_expr ().accept_vis (*this);
}

void
PatternChecker::visit (TraitBound &)
{}

void
PatternChecker::visit (ImplTraitType &)
{}

void
PatternChecker::visit (TraitObjectType &)
{}

void
PatternChecker::visit (ParenthesisedType &)
{}

void
PatternChecker::visit (TupleType &)
{}

void
PatternChecker::visit (NeverType &)
{}

void
PatternChecker::visit (RawPointerType &)
{}

void
PatternChecker::visit (ReferenceType &)
{}

void
PatternChecker::visit (ArrayType &)
{}

void
PatternChecker::visit (SliceType &)
{}

void
PatternChecker::visit (InferredType &)
{}

void
PatternChecker::visit (BareFunctionType &)
{}

bool
Constructor::is_covered_by (const Constructor &o) const
{
  if (o.kind == ConstructorKind::WILDCARD)
    return true;

  switch (kind)
    {
      case ConstructorKind::VARIANT: {
	rust_assert (kind == ConstructorKind::VARIANT);
	return variant_idx == o.variant_idx;
      }
      break;
      case ConstructorKind::INT_RANGE: {
	rust_assert (kind == ConstructorKind::INT_RANGE);
	return int_range.lo >= o.int_range.lo && int_range.hi <= o.int_range.hi;
      }
      break;
      case ConstructorKind::WILDCARD: {
	// TODO: wildcard is covered by a variant of enum with a single
	// variant
	return false;
      }
      break;
      case ConstructorKind::STRUCT: {
	// Struct pattern is always covered by a other struct constructor.
	return true;
      }
      break;
      // TODO: support references
    case ConstructorKind::REFERENCE:
    default:
      rust_unreachable ();
    }
}

bool
Constructor::operator< (const Constructor &o) const
{
  if (kind != o.kind)
    return kind < o.kind;

  switch (kind)
    {
    case ConstructorKind::VARIANT:
      return variant_idx < o.variant_idx;
    case ConstructorKind::INT_RANGE:
      return int_range.lo < o.int_range.lo
	     || (int_range.lo == o.int_range.lo
		 && int_range.hi < o.int_range.hi);
    case ConstructorKind::STRUCT:
    case ConstructorKind::WILDCARD:
    case ConstructorKind::REFERENCE:
      return false;
    default:
      rust_unreachable ();
    }
}

std::string
Constructor::to_string () const
{
  switch (kind)
    {
    case ConstructorKind::STRUCT:
      return "STRUCT";
    case ConstructorKind::VARIANT:
      return "VARIANT(" + std::to_string (variant_idx) + ")";
    case ConstructorKind::INT_RANGE:
      return "RANGE" + std::to_string (int_range.lo) + ".."
	     + std::to_string (int_range.hi);
    case ConstructorKind::WILDCARD:
      return "_";
    case ConstructorKind::REFERENCE:
      return "REF";
    default:
      rust_unreachable ();
    }
}

std::vector<DeconstructedPat>
DeconstructedPat::specialize (const Constructor &other_ctor,
			      int other_ctor_arity) const
{
  rust_assert (other_ctor.is_covered_by (ctor));
  if (ctor.is_wildcard ())
    return std::vector<DeconstructedPat> (
      other_ctor_arity,
      DeconstructedPat (Constructor::make_wildcard (), locus));

  return fields;
}

std::string
DeconstructedPat::to_string () const
{
  std::string s = ctor.to_string () + "[";
  for (auto &f : fields)
    s += f.to_string () + ", ";

  s += "](arity=" + std::to_string (arity) + ")";
  return s;
}

bool
PatOrWild::is_covered_by (const Constructor &c) const
{
  if (pat.has_value ())
    return pat.value ().get_ctor ().is_covered_by (c);
  else
    return true;
}

std::vector<PatOrWild>
PatOrWild::specialize (const Constructor &other_ctor,
		       int other_ctor_arity) const
{
  if (pat.has_value ())
    {
      auto v = pat.value ().specialize (other_ctor, other_ctor_arity);
      std::vector<PatOrWild> ret;
      for (auto &pat : v)
	ret.push_back (PatOrWild::make_pattern (pat));

      return ret;
    }
  else
    {
      return std::vector<PatOrWild> (other_ctor_arity,
				     PatOrWild::make_wildcard ());
    }
}

std::string
PatOrWild::to_string () const
{
  if (pat.has_value ())
    return pat.value ().to_string ();
  else
    return "Wild";
}

void
PatStack::pop_head_constructor (const Constructor &other_ctor,
				int other_ctor_arity)
{
  rust_assert (!pats.empty ());
  rust_assert (other_ctor.is_covered_by (head ().ctor ()));

  PatOrWild &hd = head ();
  auto v = hd.specialize (other_ctor, other_ctor_arity);
  {
    std::string s = "[";
    for (auto &pat : v)
      s += pat.to_string () + ", ";
    s += "]";

    rust_debug ("specialize %s with %s to %s", hd.to_string ().c_str (),
		other_ctor.to_string ().c_str (), s.c_str ());
  }
  pop_head ();
  for (auto &pat : v)
    pats.push_back (pat);
}

std::string
MatrixRow::to_string () const
{
  std::string s;
  for (const PatOrWild &pat : pats.get_subpatterns ())
    s += pat.to_string () + ", ";
  return s;
}

std::vector<PlaceInfo>
PlaceInfo::specialize (const Constructor &c) const
{
  switch (c.get_kind ())
    {
    case Constructor::ConstructorKind::WILDCARD:
      case Constructor::ConstructorKind::INT_RANGE: {
	return {};
      }
      break;
    case Constructor::ConstructorKind::STRUCT:
      case Constructor::ConstructorKind::VARIANT: {
	rust_assert (ty->get_kind () == TyTy::TypeKind::ADT);
	TyTy::ADTType *adt = static_cast<TyTy::ADTType *> (ty);
	switch (adt->get_adt_kind ())
	  {
	  case TyTy::ADTType::ADTKind::ENUM:
	  case TyTy::ADTType::ADTKind::STRUCT_STRUCT:
	    case TyTy::ADTType::ADTKind::TUPLE_STRUCT: {
	      TyTy::VariantDef *variant
		= adt->get_variants ().at (c.get_variant_index ());
	      if (variant->get_variant_type ()
		  == TyTy::VariantDef::VariantType::NUM)
		return {};

	      std::vector<PlaceInfo> new_place_infos;
	      for (auto &field : variant->get_fields ())
		new_place_infos.push_back (field->get_field_type ());

	      return new_place_infos;
	    }
	    break;
	    case TyTy::ADTType::ADTKind::UNION: {
	      // TODO: support unions
	      rust_unreachable ();
	    }
	  }
      }
      break;
      default: {
	rust_unreachable ();
      }
      break;
    }

  rust_unreachable ();
}

Matrix
Matrix::specialize (const Constructor &ctor) const
{
  auto subfields_place_info = place_infos.at (0).specialize (ctor);

  std::vector<MatrixRow> new_rows;
  for (const MatrixRow &row : rows)
    {
      PatStack pats = row.get_pats_clone ();
      const PatOrWild &hd = pats.head ();
      if (ctor.is_covered_by (hd.ctor ()))
	{
	  pats.pop_head_constructor (ctor, subfields_place_info.size ());
	  new_rows.push_back (MatrixRow (pats, row.is_under_guard ()));
	}
    }

  if (place_infos.empty ())
    return Matrix (new_rows, {});

  // push subfields of the first fields after specialization
  std::vector<PlaceInfo> new_place_infos = subfields_place_info;
  // add place infos for the rest of the fields
  for (size_t i = 1; i < place_infos.size (); i++)
    new_place_infos.push_back (place_infos.at (i));

  return Matrix (new_rows, new_place_infos);
}

std::string
Matrix::to_string () const
{
  std::string s = "[\n";
  for (const MatrixRow &row : rows)
    s += "row: " + row.to_string () + "\n";

  s += "](place_infos=[";
  for (const PlaceInfo &place_info : place_infos)
    s += place_info.get_type ()->as_string () + ", ";

  s += "])";
  return s;
}

std::string
WitnessPat::to_string () const
{
  switch (ctor.get_kind ())
    {
      case Constructor::ConstructorKind::STRUCT: {
	TyTy::ADTType *adt = static_cast<TyTy::ADTType *> (ty);
	TyTy::VariantDef *variant
	  = adt->get_variants ().at (ctor.get_variant_index ());
	std::string buf;
	buf += adt->get_identifier ();

	buf += " {";
	if (!fields.empty ())
	  buf += " ";

	for (size_t i = 0; i < fields.size (); i++)
	  {
	    buf += variant->get_fields ().at (i)->get_name () + ": ";
	    buf += fields.at (i).to_string ();
	    if (i < fields.size () - 1)
	      buf += ", ";
	  }
	if (!fields.empty ())
	  buf += " ";

	buf += "}";
	return buf;
      }
      break;
      case Constructor::ConstructorKind::VARIANT: {
	std::string buf;
	TyTy::ADTType *adt = static_cast<TyTy::ADTType *> (ty);
	buf += adt->get_identifier ();
	TyTy::VariantDef *variant
	  = adt->get_variants ().at (ctor.get_variant_index ());
	buf += "::" + variant->get_identifier ();

	switch (variant->get_variant_type ())
	  {
	    case TyTy::VariantDef::VariantType::NUM: {
	      return buf;
	    }
	    break;
	    case TyTy::VariantDef::VariantType::TUPLE: {
	      buf += "(";
	      for (size_t i = 0; i < fields.size (); i++)
		{
		  buf += fields.at (i).to_string ();
		  if (i < fields.size () - 1)
		    buf += ", ";
		}
	      buf += ")";
	      return buf;
	    }
	    break;
	    case TyTy::VariantDef::VariantType::STRUCT: {
	      buf += " {";
	      if (!fields.empty ())
		buf += " ";

	      for (size_t i = 0; i < fields.size (); i++)
		{
		  buf += variant->get_fields ().at (i)->get_name () + ": ";
		  buf += fields.at (i).to_string ();
		  if (i < fields.size () - 1)
		    buf += ", ";
		}

	      if (!fields.empty ())
		buf += " ";

	      buf += "}";
	    }
	    break;
	    default: {
	      rust_unreachable ();
	    }
	    break;
	  }
	return buf;
      }
      break;
      case Constructor::ConstructorKind::INT_RANGE: {
	// TODO: implement
	rust_unreachable ();
      }
      break;
      case Constructor::ConstructorKind::WILDCARD: {
	return "_";
      }
      break;
      case Constructor::ConstructorKind::REFERENCE: {
	// TODO: implement
	rust_unreachable ();
      }
      break;
      default: {
	rust_unreachable ();
      }
      break;
    }
  rust_unreachable ();
}

void
WitnessMatrix::apply_constructor (const Constructor &ctor,
				  const std::set<Constructor> &missings,
				  TyTy::BaseType *ty)
{
  int arity = 0;
  // TODO: only support struct and variant ctor for now.
  switch (ctor.get_kind ())
    {
      case Constructor::ConstructorKind::WILDCARD: {
	arity = 0;
      }
      break;
    case Constructor::ConstructorKind::STRUCT:
      case Constructor::ConstructorKind::VARIANT: {
	if (ty->get_kind () == TyTy::TypeKind::ADT)
	  {
	    TyTy::ADTType *adt = static_cast<TyTy::ADTType *> (ty);
	    TyTy::VariantDef *variant
	      = adt->get_variants ().at (ctor.get_variant_index ());
	    if (variant->get_variant_type () == TyTy::VariantDef::NUM)
	      arity = 0;
	    else
	      arity = variant->get_fields ().size ();
	  }
      }
      break;
      default: {
	rust_unreachable ();
      }
    }

  std::string buf;
  for (auto &stack : patstacks)
    {
      buf += "[";
      for (auto &pat : stack)
	buf += pat.to_string () + ", ";

      buf += "]\n";
    }
  rust_debug ("witness pats:\n%s", buf.c_str ());

  for (auto &stack : patstacks)
    {
      std::vector<WitnessPat> subfield;
      for (int i = 0; i < arity; i++)
	{
	  if (stack.empty ())
	    subfield.push_back (WitnessPat::make_wildcard (ty));
	  else
	    {
	      subfield.push_back (stack.back ());
	      stack.pop_back ();
	    }
	}

      stack.push_back (WitnessPat (ctor, subfield, ty));
    }
}

void
WitnessMatrix::extend (const WitnessMatrix &other)
{
  patstacks.insert (patstacks.end (), other.patstacks.begin (),
		    other.patstacks.end ());
}

// forward declarations
static DeconstructedPat
lower_pattern (Resolver::TypeCheckContext *ctx, HIR::Pattern &pattern,
	       TyTy::BaseType *scrutinee_ty);

static DeconstructedPat
lower_tuple_pattern (Resolver::TypeCheckContext *ctx,
		     HIR::TupleStructPattern &pattern,
		     TyTy::VariantDef *variant, Constructor &ctor)
{
  int arity = variant->get_fields ().size ();
  HIR::TupleStructItems &elems = pattern.get_items ();

  std::vector<DeconstructedPat> fields;
  switch (elems.get_item_type ())
    {
      case HIR::TupleStructItems::ItemType::MULTIPLE: {
	HIR::TupleStructItemsNoRange &multiple
	  = static_cast<HIR::TupleStructItemsNoRange &> (elems);

	rust_assert (variant->get_fields ().size ()
		     == multiple.get_patterns ().size ());

	for (size_t i = 0; i < multiple.get_patterns ().size (); i++)
	  {
	    fields.push_back (
	      lower_pattern (ctx, *multiple.get_patterns ().at (i),
			     variant->get_fields ().at (i)->get_field_type ()));
	  }
	return DeconstructedPat (ctor, arity, fields, pattern.get_locus ());
      }
      break;
      case HIR::TupleStructItems::ItemType::RANGED: {
	// TODO: ranged tuple struct items
	rust_unreachable ();
      }
      break;
      default: {
	rust_unreachable ();
      }
    }
}

static DeconstructedPat
lower_struct_pattern (Resolver::TypeCheckContext *ctx,
		      HIR::StructPattern &pattern, TyTy::VariantDef *variant,
		      Constructor ctor)
{
  int arity = variant->get_fields ().size ();

  // Initialize all field patterns to wildcard.
  std::vector<DeconstructedPat> fields
    = std::vector<DeconstructedPat> (arity, DeconstructedPat::make_wildcard (
					      pattern.get_locus ()));

  std::map<std::string, int> field_map;
  for (int i = 0; i < arity; i++)
    {
      auto &f = variant->get_fields ().at (i);
      field_map[f->get_name ()] = i;
    }

  // Fill in the fields with the present patterns.
  HIR::StructPatternElements elems = pattern.get_struct_pattern_elems ();
  for (auto &elem : elems.get_struct_pattern_fields ())
    {
      switch (elem->get_item_type ())
	{
	  case HIR::StructPatternField::ItemType::IDENT: {
	    HIR::StructPatternFieldIdent *ident
	      = static_cast<HIR::StructPatternFieldIdent *> (elem.get ());
	    int field_idx
	      = field_map.at (ident->get_identifier ().as_string ());
	    fields.at (field_idx)
	      = DeconstructedPat::make_wildcard (pattern.get_locus ());
	  }
	  break;
	  case HIR::StructPatternField::ItemType::IDENT_PAT: {
	    HIR::StructPatternFieldIdentPat *ident_pat
	      = static_cast<HIR::StructPatternFieldIdentPat *> (elem.get ());
	    int field_idx
	      = field_map.at (ident_pat->get_identifier ().as_string ());
	    fields.at (field_idx) = lower_pattern (
	      ctx, ident_pat->get_pattern (),
	      variant->get_fields ().at (field_idx)->get_field_type ());
	  }
	  break;
	  case HIR::StructPatternField::ItemType::TUPLE_PAT: {
	    // TODO: tuple: pat
	    rust_unreachable ();
	  }
	  break;
	  default: {
	    rust_unreachable ();
	  }
	}
    }

  return DeconstructedPat{ctor, arity, fields, pattern.get_locus ()};
};

static DeconstructedPat
lower_pattern (Resolver::TypeCheckContext *ctx, HIR::Pattern &pattern,
	       TyTy::BaseType *scrutinee_ty)
{
  HIR::Pattern::PatternType pat_type = pattern.get_pattern_type ();
  switch (pat_type)
    {
    case HIR::Pattern::PatternType::WILDCARD:
      case HIR::Pattern::PatternType::IDENTIFIER: {
	return DeconstructedPat::make_wildcard (pattern.get_locus ());
      }
      break;
      case HIR::Pattern::PatternType::PATH: {
	// TODO: support constants, associated constants, enum variants and
	// structs
	// https://doc.rust-lang.org/reference/patterns.html#path-patterns
	// unimplemented. Treat this pattern as wildcard for now.
	return DeconstructedPat::make_wildcard (pattern.get_locus ());
      }
      break;
      case HIR::Pattern::PatternType::REFERENCE: {
	// TODO: unimplemented. Treat this pattern as wildcard for now.
	return DeconstructedPat::make_wildcard (pattern.get_locus ());
      }
      break;
    case HIR::Pattern::PatternType::STRUCT:
      case HIR::Pattern::PatternType::TUPLE_STRUCT: {
	HirId path_id = UNKNOWN_HIRID;
	if (pat_type == HIR::Pattern::PatternType::STRUCT)
	  {
	    HIR::StructPattern &struct_pattern
	      = static_cast<HIR::StructPattern &> (pattern);
	    path_id = struct_pattern.get_path ().get_mappings ().get_hirid ();
	  }
	else
	  {
	    HIR::TupleStructPattern &tuple_pattern
	      = static_cast<HIR::TupleStructPattern &> (pattern);
	    path_id = tuple_pattern.get_path ().get_mappings ().get_hirid ();
	  }

	rust_assert (scrutinee_ty->get_kind () == TyTy::TypeKind::ADT);
	TyTy::ADTType *adt = static_cast<TyTy::ADTType *> (scrutinee_ty);

	Constructor ctor = Constructor::make_struct ();
	TyTy::VariantDef *variant;
	if (adt->is_struct_struct () || adt->is_tuple_struct ())
	  variant = adt->get_variants ().at (0);
	else if (adt->is_enum ())
	  {
	    HirId variant_id = UNKNOWN_HIRID;
	    bool ok = ctx->lookup_variant_definition (path_id, &variant_id);
	    rust_assert (ok);

	    int variant_idx;
	    ok = adt->lookup_variant_by_id (variant_id, &variant, &variant_idx);
	    rust_assert (ok);

	    ctor = Constructor::make_variant (variant_idx);
	  }
	else
	  {
	    rust_unreachable ();
	  }
	rust_assert (variant->get_variant_type ()
		       == TyTy::VariantDef::VariantType::TUPLE
		     || variant->get_variant_type ()
			  == TyTy::VariantDef::VariantType::STRUCT);

	if (pat_type == HIR::Pattern::PatternType::STRUCT)
	  {
	    HIR::StructPattern &struct_pattern
	      = static_cast<HIR::StructPattern &> (pattern);
	    return lower_struct_pattern (ctx, struct_pattern, variant, ctor);
	  }
	else
	  {
	    HIR::TupleStructPattern &tuple_pattern
	      = static_cast<HIR::TupleStructPattern &> (pattern);
	    return lower_tuple_pattern (ctx, tuple_pattern, variant, ctor);
	  }
      }
      break;
      case HIR::Pattern::PatternType::TUPLE: {
	// TODO: unimplemented. Treat this pattern as wildcard for now.
	return DeconstructedPat::make_wildcard (pattern.get_locus ());
      }
      break;
      case HIR::Pattern::PatternType::SLICE: {
	// TODO: unimplemented. Treat this pattern as wildcard for now.
	return DeconstructedPat::make_wildcard (pattern.get_locus ());
      }
      break;
      case HIR::Pattern::PatternType::ALT: {
	// TODO: unimplemented. Treat this pattern as wildcard for now.
	return DeconstructedPat::make_wildcard (pattern.get_locus ());
      }
      break;
      case HIR::Pattern::PatternType::LITERAL: {
	// TODO: unimplemented. Treat this pattern as wildcard for now.
	return DeconstructedPat::make_wildcard (pattern.get_locus ());
      }
      break;
      case HIR::Pattern::PatternType::RANGE: {
	// TODO: unimplemented. Treat this pattern as wildcard for now.
	return DeconstructedPat::make_wildcard (pattern.get_locus ());
      }
      break;
      case HIR::Pattern::PatternType::GROUPED: {
	// TODO: unimplemented. Treat this pattern as wildcard for now.
	return DeconstructedPat::make_wildcard (pattern.get_locus ());
      }
      break;
      default: {
	rust_unreachable ();
      }
    }
}

static MatchArm
lower_arm (Resolver::TypeCheckContext *ctx, HIR::MatchCase &arm,
	   TyTy::BaseType *scrutinee_ty)
{
  rust_assert (arm.get_arm ().get_patterns ().size () > 0);

  DeconstructedPat pat
    = lower_pattern (ctx, *arm.get_arm ().get_patterns ().at (0), scrutinee_ty);
  return MatchArm (pat, arm.get_arm ().has_match_arm_guard ());
}

std::pair<std::set<Constructor>, std::set<Constructor>>
split_constructors (std::vector<Constructor> &ctors, PlaceInfo &place_info)
{
  bool all_wildcard = true;
  for (auto &ctor : ctors)
    {
      if (!ctor.is_wildcard ())
	all_wildcard = false;
    }

  // first pass for the case that all patterns are wildcard
  if (all_wildcard)
    return std::make_pair (std::set<Constructor> (
			     {Constructor::make_wildcard ()}),
			   std::set<Constructor> ());

  // TODO: only support enums and structs for now.
  TyTy::BaseType *ty = place_info.get_type ();
  rust_assert (ty->get_kind () == TyTy::TypeKind::ADT);
  TyTy::ADTType *adt = static_cast<TyTy::ADTType *> (ty);
  rust_assert (adt->is_enum () || adt->is_struct_struct ()
	       || adt->is_tuple_struct ());

  std::set<Constructor> universe;
  if (adt->is_enum ())
    {
      for (size_t i = 0; i < adt->get_variants ().size (); i++)
	universe.insert (Constructor::make_variant (i));
    }
  else if (adt->is_struct_struct () || adt->is_tuple_struct ())
    {
      universe.insert (Constructor::make_struct ());
    }

  std::set<Constructor> present;
  for (auto &ctor : ctors)
    {
      if (ctor.is_wildcard ())
	return std::make_pair (universe, std::set<Constructor> ());
      else
	present.insert (ctor);
    }

  std::set<Constructor> missing;
  std::set_difference (universe.begin (), universe.end (), present.begin (),
		       present.end (), std::inserter (missing, missing.end ()));
  return std::make_pair (universe, missing);
}

// The core of the algorithm. It computes the usefulness and exhaustiveness of a
// given matrix recursively.
// TODO: calculate usefulness
static WitnessMatrix
compute_exhaustiveness_and_usefulness (Resolver::TypeCheckContext *ctx,
				       Matrix &matrix)
{
  rust_debug ("call compute_exhaustiveness_and_usefulness");
  rust_debug ("matrix: %s", matrix.to_string ().c_str ());

  if (matrix.get_rows ().empty ())
    {
      // no rows left. This means a non-exhaustive pattern.
      rust_debug ("non-exhaustive subpattern found");
      return WitnessMatrix::make_unit ();
    }

  // Base case: there are no columns in matrix.
  if (matrix.get_place_infos ().empty ())
    return WitnessMatrix::make_empty ();

  std::vector<Constructor> heads;
  for (auto head : matrix.heads ())
    heads.push_back (head.ctor ());

  // TODO: not sure missing ctors need to be calculated
  auto ctors_and_missings
    = split_constructors (heads, matrix.get_place_infos ().at (0));
  std::set<Constructor> ctors = ctors_and_missings.first;
  std::set<Constructor> missings = ctors_and_missings.second;

  WitnessMatrix ret = WitnessMatrix::make_empty ();
  for (auto &ctor : ctors)
    {
      rust_debug ("specialize with %s", ctor.to_string ().c_str ());
      // TODO: Instead of creating new matrix, we can change the original matrix
      // and use it for sub-pattern matching. It will significantly reduce
      // memory usage.
      Matrix spec_matrix = matrix.specialize (ctor);

      WitnessMatrix witness
	= compute_exhaustiveness_and_usefulness (ctx, spec_matrix);

      TyTy::BaseType *ty = matrix.get_place_infos ().at (0).get_type ();
      witness.apply_constructor (ctor, missings, ty);
      ret.extend (witness);
    }

  return ret;
}

static void
emit_exhaustiveness_error (Resolver::TypeCheckContext *ctx,
			   HIR::MatchExpr &expr, WitnessMatrix &witness)
{
  TyTy::BaseType *scrutinee_ty;
  bool ok
    = ctx->lookup_type (expr.get_scrutinee_expr ().get_mappings ().get_hirid (),
			&scrutinee_ty);
  rust_assert (ok);

  if (!witness.empty ())
    {
      std::stringstream buf;
      for (size_t i = 0; i < witness.get_stacks ().size (); i++)
	{
	  auto &stack = witness.get_stacks ().at (i);
	  WitnessPat w = WitnessPat::make_wildcard (scrutinee_ty);
	  if (!stack.empty ())
	    w = stack.at (0);

	  rust_debug ("Witness[%d]: %s", (int) i, w.to_string ().c_str ());
	  buf << "'" << w.to_string () << "'";
	  if (i != witness.get_stacks ().size () - 1)
	    buf << " and ";
	}
      rust_error_at (expr.get_scrutinee_expr ().get_locus (),
		     "non-exhaustive patterns: %s not covered",
		     buf.str ().c_str ());
    }
  else
    {
      rust_debug ("no witness found");
    }
}

// Entry point for computing match usefulness and check exhaustiveness
void
check_match_usefulness (Resolver::TypeCheckContext *ctx,
			TyTy::BaseType *scrutinee_ty, HIR::MatchExpr &expr)
{
  if (!expr.has_match_arms ())
    return;

  // Lower the arms to a more convenient representation.
  std::vector<MatrixRow> rows;
  for (auto &arm : expr.get_match_cases ())
    {
      PatStack pats;
      MatchArm lowered = lower_arm (ctx, arm, scrutinee_ty);
      PatOrWild pat = PatOrWild::make_pattern (lowered.get_pat ());
      pats.push (pat);
      rows.push_back (MatrixRow (pats, lowered.has_guard ()));
    }

  std::vector<PlaceInfo> place_infos = {{PlaceInfo (scrutinee_ty)}};
  Matrix matrix{rows, place_infos};

  WitnessMatrix witness = compute_exhaustiveness_and_usefulness (ctx, matrix);

  emit_exhaustiveness_error (ctx, expr, witness);
}

} // namespace Analysis
} // namespace Rust
