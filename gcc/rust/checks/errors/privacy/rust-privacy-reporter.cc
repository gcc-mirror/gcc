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

#include "rust-privacy-reporter.h"
#include "rust-session-manager.h"
#include "rust-hir-expr.h"
#include "rust-hir-stmt.h"
#include "rust-hir-item.h"
#include "rust-attribute-values.h"
#include "rust-immutable-name-resolution-context.h"

namespace Rust {
namespace Privacy {

PrivacyReporter::PrivacyReporter (
  Analysis::Mappings &mappings, Resolver::Resolver &resolver,
  const Rust::Resolver::TypeCheckContext &ty_ctx)
  : mappings (mappings), resolver (resolver), ty_ctx (ty_ctx),
    current_module (tl::nullopt)
{}

// Find a proc_macro, proc_macro_derive or proc_macro_attribute
// attribute in a vector of attribute
static tl::optional<std::string>
find_proc_macro_attribute (const AST::AttrVec &outer_attrs)
{
  for (auto &a : outer_attrs)
    {
      auto &segments = a.get_path ().get_segments ();
      if (segments.size () != 1)
	continue;
      auto name = segments.at (0).get_segment_name ();
      if (name == Values::Attributes::PROC_MACRO
	  || name == Values::Attributes::PROC_MACRO_ATTRIBUTE
	  || name == Values::Attributes::PROC_MACRO_DERIVE)
	return name;
    }

  return tl::nullopt;
}

// Common check on crate items when dealing with 'proc-macro' crate type.
static void
proc_macro_privacy_check (std::unique_ptr<HIR::Item> &item)
{
  if (item->get_hir_kind () == HIR::Node::BaseKind::VIS_ITEM)
    {
      auto attribute = find_proc_macro_attribute (item->get_outer_attrs ());

      bool pub_item = static_cast<HIR::VisItem *> (item.get ())
			->get_visibility ()
			.is_public ();

      if (pub_item && !attribute.has_value ()) // Public non proc-macro
	rust_error_at (
	  item->get_locus (),
	  "%<proc-macro%> crate types currently cannot export any "
	  "items other than functions tagged with %<#[proc_macro]%>, "
	  "%<#[proc_macro_derive]%> or %<#[proc_macro_attribute]%>");
      else if (!pub_item && attribute.has_value ()) // Private proc-macro
	rust_error_at (item->get_locus (),
		       "functions tagged with %<#[%s]%> must be %<pub%>",
		       attribute.value ().c_str ());
    }
}

void
PrivacyReporter::go (HIR::Crate &crate)
{
  for (auto &item : crate.get_items ())
    {
      if (Session::get_instance ().options.is_proc_macro ())
	proc_macro_privacy_check (item);
      item->accept_vis (*this);
    }
}

static bool
is_child_module (Analysis::Mappings &mappings, NodeId parent,
		 NodeId possible_child)
{
  if (flag_name_resolution_2_0)
    {
      auto &nr_ctx
	= Resolver2_0::ImmutableNameResolutionContext::get ().resolver ();

      return nr_ctx.values.is_module_descendant (parent, possible_child);
    }

  auto children = mappings.lookup_module_children (parent);

  if (!children)
    return false;

  // Visit all toplevel children
  for (auto &child : *children)
    if (child == possible_child)
      return true;

  // Now descend recursively in the child module tree
  for (auto &child : *children)
    if (is_child_module (mappings, child, possible_child))
      return true;

  return false;
}

// FIXME: This function needs a lot of refactoring
void
PrivacyReporter::check_for_privacy_violation (const NodeId &use_id,
					      const location_t locus)
{
  NodeId ref_node_id = UNKNOWN_NODEID;

  if (flag_name_resolution_2_0)
    {
      auto &nr_ctx
	= Resolver2_0::ImmutableNameResolutionContext::get ().resolver ();

      if (auto id = nr_ctx.lookup (use_id))
	ref_node_id = *id;
    }
  // FIXME: Don't assert here - we might be dealing with a type
  else if (!resolver.lookup_resolved_name (use_id, &ref_node_id))
    resolver.lookup_resolved_type (use_id, &ref_node_id);

  // FIXME: Assert here. For now, we return since this causes issues when
  // checking inferred types (#1260)
  // rust_assert (ref_node_id != UNKNOWN_NODEID);
  if (ref_node_id == UNKNOWN_NODEID)
    return;

  auto vis = mappings.lookup_visibility (ref_node_id);

  // FIXME: Can we really return here if the item has no visibility?
  if (!vis)
    return;

  auto valid = true;

  switch (vis->get_kind ())
    {
    case ModuleVisibility::Public:
      break;
      case ModuleVisibility::Restricted: {
	// If we are in the crate, everything is restricted correctly, but we
	// can't get a module for it
	if (!current_module.has_value ())
	  return;

	auto module = mappings.lookup_defid (vis->get_module_id ()).value ();

	auto mod_node_id = module->get_mappings ().get_nodeid ();

	// We are in the module referenced by the pub(restricted) visibility.
	// This is valid
	if (mod_node_id == current_module.value ())
	  break;

	// FIXME: This needs a LOT of TLC: hinting about the definition, a
	// string to say if it's a module, function, type, etc...
	if (!is_child_module (mappings, mod_node_id, current_module.value ()))
	  valid = false;
      }
      break;
    case ModuleVisibility::Unknown:
      rust_unreachable ();
      break;
    }

  if (!valid)
    {
      rich_location richloc (line_table, locus);
      richloc.add_fixit_replace ("item is private");
      rust_error_at (richloc, ErrorCode::E0603,
		     "definition is private in this context");
    }
}

void
PrivacyReporter::check_base_type_privacy (Analysis::NodeMapping &node_mappings,
					  const TyTy::BaseType *ty,
					  const location_t locus)
{
  // Avoids repeating commong argument such as `use_id` or `locus` since we're
  // doing a lot of recursive calls here
  auto recursive_check
    = [this, &node_mappings, &locus] (const TyTy::BaseType *ty) {
	return check_base_type_privacy (node_mappings, ty, locus);
      };

  switch (ty->get_kind ())
    {
      // These "simple" types are our stop condition
    case TyTy::BOOL:
    case TyTy::CHAR:
    case TyTy::INT:
    case TyTy::UINT:
    case TyTy::FLOAT:
    case TyTy::USIZE:
    case TyTy::ISIZE:
    case TyTy::ADT:
      case TyTy::STR: {
	auto ref_id = ty->get_ref ();
	if (auto lookup_id = mappings.lookup_hir_to_node (ref_id))
	  return check_for_privacy_violation (*lookup_id, locus);
	rust_unreachable ();
      }
    case TyTy::REF:
      return recursive_check (
	static_cast<const TyTy::ReferenceType *> (ty)->get_base ());
    case TyTy::POINTER:
      return recursive_check (
	static_cast<const TyTy::PointerType *> (ty)->get_base ());
    case TyTy::ARRAY:
      return recursive_check (
	static_cast<const TyTy::ArrayType *> (ty)->get_element_type ());
    case TyTy::SLICE:
      return recursive_check (
	static_cast<const TyTy::SliceType *> (ty)->get_element_type ());
    case TyTy::FNPTR:
      for (auto &param : static_cast<const TyTy::FnPtr *> (ty)->get_params ())
	recursive_check (param.get_tyty ());
      return recursive_check (
	static_cast<const TyTy::FnPtr *> (ty)->get_return_type ());
    case TyTy::TUPLE:
      for (auto &param :
	   static_cast<const TyTy::TupleType *> (ty)->get_fields ())
	recursive_check (param.get_tyty ());
      return;
      case TyTy::PLACEHOLDER: {
	const auto p = static_cast<const TyTy::PlaceholderType *> (ty);
	if (!p->can_resolve ())
	  return;
	return recursive_check (p->resolve ());
      }
    case TyTy::PROJECTION:
      return recursive_check (
	static_cast<const TyTy::ProjectionType *> (ty)->get ());
    case TyTy::CLOSURE:
      rust_sorry_at (locus, "privacy pass for closures is not handled yet");
      break;

      // If we're dealing with a generic param, there's nothing we should be
      // doing here
    case TyTy::PARAM:
      // We are dealing with a function definition that has been assigned
      // somewhere else. Nothing to resolve privacy-wise other than the actual
      // function, which is resolved as an expression
    case TyTy::FNDEF:
      // FIXME: Can we really not resolve Dynamic types here? Shouldn't we have
      // a look at the path and perform proper privacy analysis?
    case TyTy::DYNAMIC:
      // The never type is builtin and always available
    case TyTy::NEVER:
      // We shouldn't have inference types here, ever
    case TyTy::INFER:
      return;
    case TyTy::OPAQUE:
      return;
    case TyTy::ERROR:
      return;
    }
}

void
PrivacyReporter::check_type_privacy (const HIR::Type &type)
{
  TyTy::BaseType *lookup = nullptr;
  rust_assert (ty_ctx.lookup_type (type.get_mappings ().get_hirid (), &lookup));

  auto node_mappings = type.get_mappings ();
  return check_base_type_privacy (node_mappings, lookup, type.get_locus ());
}

void
PrivacyReporter::visit (HIR::PathInExpression &path)
{
  check_for_privacy_violation (path.get_mappings ().get_nodeid (),
			       path.get_locus ());
}

void
PrivacyReporter::visit (HIR::TypePathSegmentFunction &)
{
  // FIXME: Do we need to do anything for this?
}

void
PrivacyReporter::visit (HIR::InlineAsm &)
{}

void
PrivacyReporter::visit (HIR::TypePath &path)
{
  check_for_privacy_violation (path.get_mappings ().get_nodeid (),
			       path.get_locus ());
}

void
PrivacyReporter::visit (HIR::QualifiedPathInExpression &path)
{
  check_for_privacy_violation (path.get_mappings ().get_nodeid (),
			       path.get_locus ());
}

void
PrivacyReporter::visit (HIR::QualifiedPathInType &path)
{
  check_for_privacy_violation (path.get_mappings ().get_nodeid (),
			       path.get_locus ());
}

void
PrivacyReporter::visit (HIR::LiteralExpr &)
{
  // Literals cannot contain any sort of privacy violation
}

void
PrivacyReporter::visit (HIR::BorrowExpr &expr)
{
  expr.get_expr ().accept_vis (*this);
}

void
PrivacyReporter::visit (HIR::DereferenceExpr &expr)
{
  expr.get_expr ().accept_vis (*this);
}

void
PrivacyReporter::visit (HIR::ErrorPropagationExpr &expr)
{
  expr.get_expr ().accept_vis (*this);
}

void
PrivacyReporter::visit (HIR::NegationExpr &expr)
{
  expr.get_expr ().accept_vis (*this);
}

void
PrivacyReporter::visit (HIR::ArithmeticOrLogicalExpr &expr)
{
  expr.get_lhs ().accept_vis (*this);
  expr.get_rhs ().accept_vis (*this);
}

void
PrivacyReporter::visit (HIR::ComparisonExpr &expr)
{
  expr.get_lhs ().accept_vis (*this);
  expr.get_rhs ().accept_vis (*this);
}

void
PrivacyReporter::visit (HIR::LazyBooleanExpr &expr)
{
  expr.get_lhs ().accept_vis (*this);
  expr.get_rhs ().accept_vis (*this);
}

void
PrivacyReporter::visit (HIR::TypeCastExpr &expr)
{
  expr.get_expr ().accept_vis (*this);
}

void
PrivacyReporter::visit (HIR::AssignmentExpr &expr)
{
  expr.get_lhs ().accept_vis (*this);
  expr.get_rhs ().accept_vis (*this);
}

void
PrivacyReporter::visit (HIR::CompoundAssignmentExpr &expr)
{
  expr.get_lhs ().accept_vis (*this);
  expr.get_rhs ().accept_vis (*this);
}

void
PrivacyReporter::visit (HIR::GroupedExpr &expr)
{
  expr.get_expr_in_parens ().accept_vis (*this);
}

void
PrivacyReporter::visit (HIR::ArrayExpr &expr)
{
  HIR::ArrayElems &elements = expr.get_internal_elements ();
  switch (elements.get_array_expr_type ())
    {
      case HIR::ArrayElems::ArrayExprType::VALUES: {
	auto &elems = static_cast<HIR::ArrayElemsValues &> (elements);
	for (auto &value : elems.get_values ())
	  value->accept_vis (*this);
      }
      return;

    case HIR::ArrayElems::ArrayExprType::COPIED:
      auto &elems = static_cast<HIR::ArrayElemsCopied &> (elements);
      elems.get_elem_to_copy ().accept_vis (*this);
    }
}

void
PrivacyReporter::visit (HIR::ArrayIndexExpr &expr)
{
  expr.get_array_expr ().accept_vis (*this);
  expr.get_index_expr ().accept_vis (*this);
}

void
PrivacyReporter::visit (HIR::TupleExpr &expr)
{
  for (auto &value : expr.get_tuple_elems ())
    value->accept_vis (*this);
}

void
PrivacyReporter::visit (HIR::TupleIndexExpr &expr)
{
  expr.get_tuple_expr ().accept_vis (*this);
}

void
PrivacyReporter::visit (HIR::StructExprStruct &)
{
  // FIXME: We need to check the visibility of the type it refers to here
}

void
PrivacyReporter::visit (HIR::StructExprFieldIdentifier &)
{}

void
PrivacyReporter::visit (HIR::StructExprFieldIdentifierValue &field)
{
  field.get_value ().accept_vis (*this);
}

void
PrivacyReporter::visit (HIR::StructExprFieldIndexValue &field)
{
  field.get_value ().accept_vis (*this);
}

void
PrivacyReporter::visit (HIR::StructExprStructFields &expr)
{
  for (auto &field : expr.get_fields ())
    field->accept_vis (*this);
}

void
PrivacyReporter::visit (HIR::CallExpr &expr)
{
  expr.get_fnexpr ().accept_vis (*this);

  for (auto &param : expr.get_arguments ())
    param->accept_vis (*this);
}

void
PrivacyReporter::visit (HIR::MethodCallExpr &expr)
{
  expr.get_receiver ().accept_vis (*this);

  for (auto &param : expr.get_arguments ())
    param->accept_vis (*this);
}

void
PrivacyReporter::visit (HIR::FieldAccessExpr &expr)
{
  expr.get_receiver_expr ().accept_vis (*this);

  // FIXME: We should also check if the field is public?
}

void
PrivacyReporter::visit (HIR::ClosureExpr &)
{
  // Not handled yet
}

void
PrivacyReporter::visit (HIR::BlockExpr &expr)
{
  for (auto &stmt : expr.get_statements ())
    stmt->accept_vis (*this);

  if (expr.has_final_expr ())
    expr.get_final_expr ().accept_vis (*this);
}

void
PrivacyReporter::visit (HIR::ContinueExpr &)
{}

void
PrivacyReporter::visit (HIR::BreakExpr &expr)
{
  if (expr.has_break_expr ())
    expr.get_expr ().accept_vis (*this);
}

void
PrivacyReporter::visit (HIR::RangeFromToExpr &expr)
{
  expr.get_from_expr ().accept_vis (*this);
  expr.get_to_expr ().accept_vis (*this);
}

void
PrivacyReporter::visit (HIR::RangeFromExpr &expr)
{
  expr.get_from_expr ().accept_vis (*this);
}

void
PrivacyReporter::visit (HIR::RangeToExpr &expr)
{
  expr.get_to_expr ().accept_vis (*this);
}

void
PrivacyReporter::visit (HIR::RangeFullExpr &)
{}

void
PrivacyReporter::visit (HIR::RangeFromToInclExpr &expr)
{
  expr.get_from_expr ().accept_vis (*this);
  expr.get_to_expr ().accept_vis (*this);
}

void
PrivacyReporter::visit (HIR::RangeToInclExpr &)
{
  // Not handled yet
}

void
PrivacyReporter::visit (HIR::ReturnExpr &expr)
{
  if (expr.has_expr ())
    expr.get_expr ().accept_vis (*this);
}

void
PrivacyReporter::visit (HIR::UnsafeBlockExpr &expr)
{
  expr.get_block_expr ().accept_vis (*this);
}

void
PrivacyReporter::visit (HIR::LoopExpr &expr)
{
  expr.get_loop_block ().accept_vis (*this);
}

void
PrivacyReporter::visit (HIR::WhileLoopExpr &expr)
{
  expr.get_predicate_expr ().accept_vis (*this);
  expr.get_loop_block ().accept_vis (*this);
}

void
PrivacyReporter::visit (HIR::WhileLetLoopExpr &expr)
{
  expr.get_cond ().accept_vis (*this);
  expr.get_loop_block ().accept_vis (*this);
}

void
PrivacyReporter::visit (HIR::IfExpr &expr)
{
  expr.get_if_condition ().accept_vis (*this);
  expr.get_if_block ().accept_vis (*this);
}

void
PrivacyReporter::visit (HIR::IfExprConseqElse &expr)
{
  expr.get_if_condition ().accept_vis (*this);
  expr.get_if_block ().accept_vis (*this);
  expr.get_else_block ().accept_vis (*this);
}

void
PrivacyReporter::visit (HIR::MatchExpr &expr)
{
  expr.get_scrutinee_expr ().accept_vis (*this);
}

void
PrivacyReporter::visit (HIR::AwaitExpr &)
{
  // Not handled yet
}

void
PrivacyReporter::visit (HIR::AsyncBlockExpr &)
{
  // Not handled yet
}

void
PrivacyReporter::visit (HIR::Module &module)
{
  // FIXME: We also need to think about module privacy

  auto old_module = current_module;
  current_module = module.get_mappings ().get_nodeid ();

  for (auto &item : module.get_items ())
    item->accept_vis (*this);

  current_module = old_module;
}

void
PrivacyReporter::visit (HIR::ExternCrate &)
{}

void
PrivacyReporter::visit (HIR::UseDeclaration &)
{
  // FIXME: Is there anything we need to do here?
}

void
PrivacyReporter::visit (HIR::Function &function)
{
  for (auto &param : function.get_function_params ())
    check_type_privacy (param.get_type ());

  function.get_definition ().accept_vis (*this);
}

void
PrivacyReporter::visit (HIR::TypeAlias &)
{
  // TODO: Check the type here
}

void
PrivacyReporter::visit (HIR::StructStruct &)
{
  // TODO: Check the type of all fields
}

void
PrivacyReporter::visit (HIR::TupleStruct &)
{
  // TODO: Check the type of all fields
}

void
PrivacyReporter::visit (HIR::EnumItem &)
{
  // TODO: Check the type of all variants
}

void
PrivacyReporter::visit (HIR::EnumItemTuple &)
{
  // TODO: Check the type
}

void
PrivacyReporter::visit (HIR::EnumItemStruct &)
{
  // TODO: Check the type
}

void
PrivacyReporter::visit (HIR::EnumItemDiscriminant &)
{}

void
PrivacyReporter::visit (HIR::Enum &)
{}

void
PrivacyReporter::visit (HIR::Union &)
{
  // TODO: Check the type
}

void
PrivacyReporter::visit (HIR::ConstantItem &const_item)
{
  // TODO: We need to visit the type
  const_item.get_expr ().accept_vis (*this);
}

void
PrivacyReporter::visit (HIR::StaticItem &static_item)
{
  // TODO: We need to visit the type
  static_item.get_expr ().accept_vis (*this);
}

void
PrivacyReporter::visit (HIR::Trait &)
{
  // FIXME: We need to be an ItemVisitor as well
  // for (auto &item : trait.get_trait_items ())
  //   item->accept_vis (*this);
}

void
PrivacyReporter::visit (HIR::ImplBlock &impl)
{
  for (auto &item : impl.get_impl_items ())
    item->accept_vis (*this);
}

void
PrivacyReporter::visit (HIR::ExternBlock &)
{
  // FIXME: We need to be an ItemVisitor as well
  // for (auto &block: block.get_extern_items ())
  //   item->accept_vis (*this);
}

void
PrivacyReporter::visit (HIR::EmptyStmt &)
{}

void
PrivacyReporter::visit (HIR::LetStmt &stmt)
{
  if (stmt.has_type ())
    check_type_privacy (stmt.get_type ());

  if (stmt.has_init_expr ())
    stmt.get_init_expr ().accept_vis (*this);
}

void
PrivacyReporter::visit (HIR::ExprStmt &stmt)
{
  stmt.get_expr ().accept_vis (*this);
}

} // namespace Privacy
} // namespace Rust
