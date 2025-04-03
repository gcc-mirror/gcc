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

#include "rust-ast-visitor.h"
#include "rust-ast-full-decls.h"
#include "rust-ast.h"
#include "rust-path.h"
#include "rust-token.h"
#include "rust-expr.h"
#include "rust-macro.h"
#include "rust-pattern.h"
#include "rust-stmt.h"
#include "rust-type.h"

namespace Rust {
namespace AST {

void
DefaultASTVisitor::visit (AST::Crate &crate)
{
  visit_inner_attrs (crate);
  for (auto &item : crate.items)
    visit (item);
}

void
DefaultASTVisitor::visit (AST::Token &tok)
{}

void
DefaultASTVisitor::visit (AST::DelimTokenTree &delim_tok_tree)
{
  for (auto &token : delim_tok_tree.get_token_trees ())
    visit (token);
}

void
DefaultASTVisitor::visit (AST::AttrInputMetaItemContainer &input)
{
  for (auto &item : input.get_items ())
    visit (item);
}

void
DefaultASTVisitor::visit (AST::IdentifierExpr &ident_expr)
{
  visit_outer_attrs (ident_expr);
}

void
DefaultASTVisitor::visit (AST::Lifetime &lifetime)
{}

void
DefaultASTVisitor::visit (AST::LifetimeParam &lifetime_param)
{
  visit_outer_attrs (lifetime_param);
  visit (lifetime_param.get_lifetime ());
  for (auto &lifetime_bound : lifetime_param.get_lifetime_bounds ())
    visit (lifetime_bound);
}

void
DefaultASTVisitor::visit (AST::ConstGenericParam &const_param)
{
  visit_outer_attrs (const_param);
  if (const_param.has_type ())
    visit (const_param.get_type ());
  if (const_param.has_default_value ())
    visit (const_param.get_default_value ());
}

void
DefaultASTVisitor::visit (AST::PathInExpression &path)
{
  visit_outer_attrs (path);

  if (!path.is_lang_item ())
    for (auto &segment : path.get_segments ())
      visit (segment);
}

void
DefaultASTVisitor::visit (AST::TypePathSegment &segment)
{}

void
DefaultASTVisitor::visit (GenericArgsBinding &binding)
{
  visit (binding.get_type ());
}

void
DefaultASTVisitor::visit (AST::TypePathSegmentGeneric &segment)
{
  visit (segment.get_generic_args ());
}

void
DefaultASTVisitor::visit (AST::TypePathFunction &tpf)
{
  for (auto &input : tpf.get_params ())
    visit (input);
  if (tpf.has_return_type ())
    visit (tpf.get_return_type ());
}

void
DefaultASTVisitor::visit (AST::PathIdentSegment &segment)
{}

void
DefaultASTVisitor::visit (AST::TypePathSegmentFunction &segment)
{
  visit (segment.get_type_path_function ());
  visit (segment.get_ident_segment ());
}

void
DefaultASTVisitor::visit (AST::GenericArgs &args)
{
  for (auto &lifetime : args.get_lifetime_args ())
    visit (lifetime);

  for (auto &generic : args.get_generic_args ())
    visit (generic);

  for (auto &binding : args.get_binding_args ())
    visit (binding);
}

void
DefaultASTVisitor::visit (AST::PathExprSegment &segment)
{
  visit (segment.get_ident_segment ());
  if (segment.has_generic_args ())
    visit (segment.get_generic_args ());
}
void
DefaultASTVisitor::visit (AST::TypePath &path)
{
  for (auto &segment : path.get_segments ())
    visit (segment);
}

void
DefaultASTVisitor::visit (AST::QualifiedPathInExpression &path)
{
  visit_outer_attrs (path);
  visit (path.get_qualified_path_type ());

  for (auto &segment : path.get_segments ())
    visit (segment);
}

void
DefaultASTVisitor::visit (AST::QualifiedPathType &path)
{
  visit (path.get_type ());
  if (path.has_as_clause ())
    visit (path.get_as_type_path ());
}

void
DefaultASTVisitor::visit (AST::QualifiedPathInType &path)
{
  visit (path.get_qualified_path_type ());
  visit (path.get_associated_segment ());

  for (auto &segment : path.get_segments ())
    visit (segment);
}

void
DefaultASTVisitor::visit (AST::LiteralExpr &expr)
{
  visit_outer_attrs (expr);
}

void
DefaultASTVisitor::visit (AST::AttrInputLiteral &attr_input)
{
  visit (attr_input.get_literal ());
}

void
DefaultASTVisitor::visit (AST::AttrInputMacro &attr_input)
{
  visit (attr_input.get_macro ());
}

void
DefaultASTVisitor::visit (AST::MetaItemLitExpr &meta_item)
{
  visit (meta_item.get_literal ());
}

void
DefaultASTVisitor::visit (AST::SimplePathSegment &segment)
{}

void
DefaultASTVisitor::visit (AST::SimplePath &path)
{
  for (auto &segment : path.get_segments ())
    visit (segment);
}

void
DefaultASTVisitor::visit (AST::MetaItemPathLit &meta_item)
{
  visit (meta_item.get_path ());
  visit (meta_item.get_literal ());
}

void
DefaultASTVisitor::visit (AST::BorrowExpr &expr)
{
  visit_outer_attrs (expr);
  visit (expr.get_borrowed_expr ());
}

void
DefaultASTVisitor::visit (AST::DereferenceExpr &expr)
{
  visit_outer_attrs (expr);
  visit (expr.get_dereferenced_expr ());
}

void
DefaultASTVisitor::visit (AST::ErrorPropagationExpr &expr)
{
  visit_outer_attrs (expr);
}

void
DefaultASTVisitor::visit (AST::NegationExpr &expr)
{
  visit_outer_attrs (expr);
  visit (expr.get_negated_expr ());
}

void
DefaultASTVisitor::visit (AST::ArithmeticOrLogicalExpr &expr)
{
  visit_outer_attrs (expr);
  visit (expr.get_left_expr ());
  visit (expr.get_right_expr ());
}

void
DefaultASTVisitor::visit (AST::ComparisonExpr &expr)
{
  visit_outer_attrs (expr);
  visit (expr.get_left_expr ());
  visit (expr.get_right_expr ());
}

void
DefaultASTVisitor::visit (AST::LazyBooleanExpr &expr)
{
  visit_outer_attrs (expr);
  visit (expr.get_left_expr ());
  visit (expr.get_right_expr ());
}

void
DefaultASTVisitor::visit (AST::TypeCastExpr &expr)
{
  visit_outer_attrs (expr);
  visit (expr.get_casted_expr ());
  visit (expr.get_type_to_cast_to ());
}

void
DefaultASTVisitor::visit (AST::AssignmentExpr &expr)
{
  visit_outer_attrs (expr);
  visit (expr.get_left_expr ());
  visit (expr.get_right_expr ());
}

void
DefaultASTVisitor::visit (AST::CompoundAssignmentExpr &expr)
{
  visit_outer_attrs (expr);
  visit (expr.get_left_expr ());
  visit (expr.get_right_expr ());
}

void
DefaultASTVisitor::visit (AST::GroupedExpr &expr)
{
  visit_outer_attrs (expr);
  visit_inner_attrs (expr);
  visit (expr.get_expr_in_parens ());
}

void
DefaultASTVisitor::visit (AST::ArrayElemsValues &elems)
{
  for (auto &value : elems.get_values ())
    visit (value);
}

void
DefaultASTVisitor::visit (AST::ArrayElemsCopied &elems)
{
  visit (elems.get_elem_to_copy ());
  visit (elems.get_num_copies ());
}

void
DefaultASTVisitor::visit (AST::ArrayExpr &expr)
{
  visit_outer_attrs (expr);
  visit_inner_attrs (expr);
  visit (expr.get_array_elems ());
}

void
DefaultASTVisitor::visit (AST::ArrayIndexExpr &expr)
{
  visit_outer_attrs (expr);
  visit (expr.get_array_expr ());
  visit (expr.get_index_expr ());
}

void
DefaultASTVisitor::visit (AST::TupleExpr &expr)
{
  visit_outer_attrs (expr);
  visit_inner_attrs (expr);
  for (auto &elem : expr.get_tuple_elems ())
    visit (elem);
}

void
DefaultASTVisitor::visit (AST::TupleIndexExpr &expr)
{
  visit_outer_attrs (expr);
  visit (expr.get_tuple_expr ());
}

void
DefaultASTVisitor::visit (AST::StructExprStruct &expr)
{
  visit_outer_attrs (expr);
  visit_inner_attrs (expr);
  visit (expr.get_struct_name ());
}

void
DefaultASTVisitor::visit (AST::StructExprFieldIdentifier &field)
{}

void
DefaultASTVisitor::visit (AST::StructExprFieldIdentifierValue &field)
{
  visit (field.get_value ());
}

void
DefaultASTVisitor::visit (AST::StructExprFieldIndexValue &field)
{
  visit (field.get_value ());
}

void
DefaultASTVisitor::visit (AST::StructBase &base)
{
  visit (base.get_base_struct ());
}

void
DefaultASTVisitor::visit (AST::StructExprStructFields &expr)
{
  visit_outer_attrs (expr);
  visit_inner_attrs (expr);
  visit (expr.get_struct_name ());
  if (expr.has_struct_base ())
    visit (expr.get_struct_base ());
  for (auto &field : expr.get_fields ())
    visit (field);
}

void
DefaultASTVisitor::visit (AST::StructExprStructBase &expr)
{
  visit_outer_attrs (expr);
  visit_inner_attrs (expr);
  visit (expr.get_struct_name ());
  visit (expr.get_struct_base ());
}

void
DefaultASTVisitor::visit (AST::CallExpr &expr)
{
  visit_outer_attrs (expr);
  visit (expr.get_function_expr ());
  for (auto &param : expr.get_params ())
    visit (param);
}

void
DefaultASTVisitor::visit (AST::MethodCallExpr &expr)
{
  visit_outer_attrs (expr);
  visit (expr.get_receiver_expr ());
  visit (expr.get_method_name ());
  for (auto &param : expr.get_params ())
    visit (param);
}

void
DefaultASTVisitor::visit (AST::FieldAccessExpr &expr)
{
  visit_outer_attrs (expr);
  visit (expr.get_receiver_expr ());
}

void
DefaultASTVisitor::visit (AST::ClosureExprInner &expr)
{
  visit_outer_attrs (expr);
  for (auto &param : expr.get_params ())
    visit (param);
  visit (expr.get_definition_expr ());
}

void
DefaultASTVisitor::visit (AST::BlockExpr &expr)
{
  visit_outer_attrs (expr);
  visit_inner_attrs (expr);
  for (auto &stmt : expr.get_statements ())
    visit (stmt);
  if (expr.has_tail_expr ())
    visit (expr.get_tail_expr ());
}

void
DefaultASTVisitor::visit (AST::ClosureExprInnerTyped &expr)
{
  visit_outer_attrs (expr);
  for (auto &param : expr.get_params ())
    visit (param);
  visit (expr.get_return_type ());
  visit (expr.get_definition_block ());
}

void
DefaultASTVisitor::visit (AST::ClosureParam &param)
{
  visit_outer_attrs (param);
  visit (param.get_pattern ());
  if (param.has_type_given ())
    visit (param.get_type ());
}

void
DefaultASTVisitor::visit (AST::ContinueExpr &expr)
{
  visit_outer_attrs (expr);
  if (expr.has_label ())
    visit (expr.get_label_unchecked ());
}

void
DefaultASTVisitor::visit (AST::BreakExpr &expr)
{
  visit_outer_attrs (expr);
  if (expr.has_label ())
    visit (expr.get_label_unchecked ());

  if (expr.has_break_expr ())
    visit (expr.get_break_expr ());
}

void
DefaultASTVisitor::visit (AST::RangeFromToExpr &expr)
{
  visit (expr.get_from_expr ());
  visit (expr.get_to_expr ());
}

void
DefaultASTVisitor::visit (AST::RangeFromExpr &expr)
{
  visit (expr.get_from_expr ());
}

void
DefaultASTVisitor::visit (AST::RangeToExpr &expr)
{
  visit (expr.get_to_expr ());
}

void
DefaultASTVisitor::visit (AST::RangeFullExpr &expr)
{}

void
DefaultASTVisitor::visit (AST::RangeFromToInclExpr &expr)
{
  visit (expr.get_from_expr ());
  visit (expr.get_to_expr ());
}

void
DefaultASTVisitor::visit (AST::RangeToInclExpr &expr)
{
  visit (expr.get_to_expr ());
}

void
DefaultASTVisitor::visit (AST::ReturnExpr &expr)
{
  visit_outer_attrs (expr);
  if (expr.has_returned_expr ())
    visit (expr.get_returned_expr ());
}

void
DefaultASTVisitor::visit (AST::BoxExpr &expr)
{
  visit_outer_attrs (expr);
  visit (expr.get_boxed_expr ());
}

void
DefaultASTVisitor::visit (AST::UnsafeBlockExpr &expr)
{
  visit_outer_attrs (expr);
  visit (expr.get_block_expr ());
}

void
DefaultASTVisitor::visit (AST::LoopLabel &label)
{
  visit (label.get_lifetime ());
}

void
DefaultASTVisitor::visit (AST::LoopExpr &expr)
{
  visit_outer_attrs (expr);
  if (expr.has_loop_label ())
    visit (expr.get_loop_label ());
  visit (expr.get_loop_block ());
}

void
DefaultASTVisitor::visit (AST::WhileLoopExpr &expr)
{
  visit_outer_attrs (expr);
  if (expr.has_loop_label ())
    visit (expr.get_loop_label ());
  visit (expr.get_predicate_expr ());
  visit (expr.get_loop_block ());
}

void
DefaultASTVisitor::visit (AST::WhileLetLoopExpr &expr)
{
  visit_outer_attrs (expr);
  for (auto &pattern : expr.get_patterns ())
    visit (pattern);
  visit (expr.get_scrutinee_expr ());
  visit (expr.get_loop_label ());
  visit (expr.get_loop_block ());
}

void
DefaultASTVisitor::visit (AST::ForLoopExpr &expr)
{
  visit_outer_attrs (expr);
  visit (expr.get_pattern ());
  visit (expr.get_iterator_expr ());
  if (expr.has_loop_label ())
    visit (expr.get_loop_label ());
  visit (expr.get_loop_block ());
}

void
DefaultASTVisitor::visit (AST::IfExpr &expr)
{
  visit_outer_attrs (expr);
  visit (expr.get_condition_expr ());
  visit (expr.get_if_block ());
}

void
DefaultASTVisitor::visit (AST::IfExprConseqElse &expr)
{
  visit (reinterpret_cast<AST::IfExpr &> (expr));
  visit (expr.get_else_block ());
}

void
DefaultASTVisitor::visit (AST::IfLetExpr &expr)
{
  visit_outer_attrs (expr);
  for (auto &pattern : expr.get_patterns ())
    visit (pattern);
  visit (expr.get_value_expr ());
  visit (expr.get_if_block ());
}

void
DefaultASTVisitor::visit (AST::IfLetExprConseqElse &expr)
{
  visit (reinterpret_cast<AST::IfLetExpr &> (expr));
  visit (expr.get_else_block ());
}

void
DefaultASTVisitor::visit (AST::MatchArm &arm)
{
  visit_outer_attrs (arm);
  for (auto &pattern : arm.get_patterns ())
    visit (pattern);
  if (arm.has_match_arm_guard ())
    visit (arm.get_guard_expr ());
}

void
DefaultASTVisitor::visit (AST::MatchCase &arm)
{
  visit (arm.get_arm ());
  visit (arm.get_expr ());
}

void
DefaultASTVisitor::visit (AST::MatchExpr &expr)
{
  visit_outer_attrs (expr);
  visit_inner_attrs (expr);
  visit (expr.get_scrutinee_expr ());
  for (auto &arm : expr.get_match_cases ())
    visit (arm);
}

void
DefaultASTVisitor::visit (AST::AwaitExpr &expr)
{
  visit_outer_attrs (expr);
  visit (expr.get_awaited_expr ());
}

void
DefaultASTVisitor::visit (AST::AsyncBlockExpr &expr)
{
  visit_outer_attrs (expr);
  visit (expr.get_block_expr ());
}

void
DefaultASTVisitor::visit (AST::InlineAsm &expr)
{
  visit_outer_attrs (expr);
  using RegisterType = AST::InlineAsmOperand::RegisterType;
  for (auto &operand : expr.get_operands ())
    {
      switch (operand.get_register_type ())
	{
	  case RegisterType::In: {
	    visit (operand.get_in ().expr);
	    break;
	  }
	  case RegisterType::Out: {
	    visit (operand.get_out ().expr);
	    break;
	  }
	  case RegisterType::InOut: {
	    visit (operand.get_in_out ().expr);
	    break;
	  }
	  case RegisterType::SplitInOut: {
	    auto split = operand.get_split_in_out ();
	    visit (split.in_expr);
	    visit (split.out_expr);
	    break;
	  }
	  case RegisterType::Const: {
	    visit (operand.get_const ().anon_const.expr);
	    break;
	  }
	  case RegisterType::Sym: {
	    visit (operand.get_sym ().expr);
	    break;
	  }
	  case RegisterType::Label: {
	    visit (operand.get_label ().expr);
	    break;
	  }
	}
    }
}

void
DefaultASTVisitor::visit (AST::TypeParam &param)
{
  visit_outer_attrs (param);
  for (auto &bound : param.get_type_param_bounds ())
    visit (bound);
  if (param.has_type ())
    visit (param.get_type ());
}

void
DefaultASTVisitor::visit (AST::LifetimeWhereClauseItem &item)
{
  visit (item.get_lifetime ());
  for (auto &bound : item.get_lifetime_bounds ())
    visit (bound);
}

void
DefaultASTVisitor::visit (AST::TypeBoundWhereClauseItem &item)
{
  for (auto &lifetime : item.get_for_lifetimes ())
    visit (lifetime);
  visit (item.get_type ());
  for (auto &param : item.get_type_param_bounds ())
    visit (param);
}

void
DefaultASTVisitor::visit (AST::Visibility &vis)
{
  visit (vis.get_path ());
}

void
DefaultASTVisitor::visit (AST::FunctionQualifiers &qualifiers)
{}

void
DefaultASTVisitor::visit (AST::WhereClause &where)
{
  for (auto &item : where.get_items ())
    visit (item);
}
void
DefaultASTVisitor::visit (AST::FunctionParam &param)
{
  visit_outer_attrs (param);
  if (param.has_name ())
    visit (param.get_pattern ());

  visit (param.get_type ());
}

void
DefaultASTVisitor::visit (AST::SelfParam &param)
{
  visit_outer_attrs (param);

  if (param.has_lifetime ())
    visit (param.get_lifetime ());

  if (param.has_type ())
    visit (param.get_type ());
}

void
DefaultASTVisitor::visit (AST::Module &module)
{
  visit_outer_attrs (module);
  visit (module.get_visibility ());
  visit_inner_attrs (module);
  for (auto &item : module.get_items ())
    visit (item);
}

void
DefaultASTVisitor::visit (AST::ExternCrate &crate)
{
  visit_outer_attrs (crate);
  visit (crate.get_visibility ());
}

void
DefaultASTVisitor::visit (AST::UseTreeGlob &use_tree)
{
  visit (use_tree.get_path ());
}

void
DefaultASTVisitor::visit (AST::UseTreeList &use_tree)
{
  visit (use_tree.get_path ());
}

void
DefaultASTVisitor::visit (AST::UseTreeRebind &use_tree)
{
  visit (use_tree.get_path ());
}

void
DefaultASTVisitor::visit (AST::UseDeclaration &use_decl)
{
  visit (use_decl.get_tree ());
}

void
DefaultASTVisitor::visit (AST::Function &function)
{
  visit_outer_attrs (function);
  visit (function.get_visibility ());
  visit (function.get_qualifiers ());
  for (auto &generic : function.get_generic_params ())
    visit (generic);
  for (auto &param : function.get_function_params ())
    visit (param);
  if (function.has_return_type ())
    visit (function.get_return_type ());
  if (function.has_where_clause ())
    visit (function.get_where_clause ());
  if (function.has_body ())
    visit (*function.get_definition ());
}

void
DefaultASTVisitor::visit (AST::TypeAlias &type_alias)
{
  visit_outer_attrs (type_alias);
  visit (type_alias.get_visibility ());
  for (auto &generic : type_alias.get_generic_params ())
    visit (generic);
  if (type_alias.has_where_clause ())
    visit (type_alias.get_where_clause ());
  visit (type_alias.get_type_aliased ());
}

void
DefaultASTVisitor::visit (AST::StructField &field)
{
  visit_outer_attrs (field);
  visit (field.get_visibility ());
  visit (field.get_field_type ());
}

void
DefaultASTVisitor::visit (AST::StructStruct &struct_item)
{
  visit_outer_attrs (struct_item);
  visit (struct_item.get_visibility ());
  for (auto &generic : struct_item.get_generic_params ())
    visit (generic);
  if (struct_item.has_where_clause ())
    visit (struct_item.get_where_clause ());
  for (auto &field : struct_item.get_fields ())
    visit (field);
}

void
DefaultASTVisitor::visit (AST::TupleField &field)
{
  visit_outer_attrs (field);
  visit (field.get_visibility ());
  visit (field.get_field_type ());
}

void
DefaultASTVisitor::visit (AST::TupleStruct &tuple_struct)
{
  visit_outer_attrs (tuple_struct);
  visit (tuple_struct.get_visibility ());
  for (auto &generic : tuple_struct.get_generic_params ())
    visit (generic);
  if (tuple_struct.has_where_clause ())
    visit (tuple_struct.get_where_clause ());
  for (auto &field : tuple_struct.get_fields ())
    visit (field);
}

void
DefaultASTVisitor::visit (AST::EnumItem &item)
{
  visit_outer_attrs (item);
  visit (item.get_visibility ());
}

void
DefaultASTVisitor::visit (AST::EnumItemTuple &item)
{
  visit (reinterpret_cast<EnumItem &> (item));
  for (auto &field : item.get_tuple_fields ())
    visit (field);
}

void
DefaultASTVisitor::visit (AST::EnumItemStruct &item)
{
  visit (reinterpret_cast<EnumItem &> (item));
  for (auto &field : item.get_struct_fields ())
    visit (field);
}

void
DefaultASTVisitor::visit (AST::EnumItemDiscriminant &item)
{
  visit (reinterpret_cast<EnumItem &> (item));
  visit (item.get_expr ());
}

void
DefaultASTVisitor::visit (AST::Enum &enum_item)
{
  visit_outer_attrs (enum_item);
  visit (enum_item.get_visibility ());
  for (auto &generic : enum_item.get_generic_params ())
    visit (generic);
  if (enum_item.has_where_clause ())
    visit (enum_item.get_where_clause ());
  for (auto &item : enum_item.get_variants ())
    visit (item);
}

void
DefaultASTVisitor::visit (AST::Union &union_item)
{
  visit_outer_attrs (union_item);
  visit (union_item.get_visibility ());
  for (auto &generic : union_item.get_generic_params ())
    visit (generic);
  if (union_item.has_where_clause ())
    visit (union_item.get_where_clause ());
  for (auto &variant : union_item.get_variants ())
    visit (variant);
}

void
DefaultASTVisitor::visit (AST::ConstantItem &const_item)
{
  visit_outer_attrs (const_item);
  visit (const_item.get_visibility ());
  visit (const_item.get_type ());
  if (const_item.has_expr ())
    visit (const_item.get_expr ());
}

void
DefaultASTVisitor::visit (AST::StaticItem &static_item)
{
  visit_outer_attrs (static_item);
  visit (static_item.get_visibility ());
  visit (static_item.get_type ());
  visit (static_item.get_expr ());
}

void
DefaultASTVisitor::visit (AST::TraitItemConst &item)
{
  visit_outer_attrs (item);
  visit (item.get_type ());
  if (item.has_expr ())
    visit (item.get_expr ());
}

void
DefaultASTVisitor::visit (AST::TraitItemType &item)
{
  visit_outer_attrs (item);
  for (auto &bound : item.get_type_param_bounds ())
    visit (bound);
}

void
DefaultASTVisitor::visit (AST::Trait &trait)
{
  visit_outer_attrs (trait);
  visit (trait.get_visibility ());

  visit_inner_attrs (trait);

  visit (trait.get_implicit_self ());

  for (auto &generic : trait.get_generic_params ())
    visit (generic);

  if (trait.has_where_clause ())
    visit (trait.get_where_clause ());

  for (auto &bound : trait.get_type_param_bounds ())
    visit (bound);

  for (auto &item : trait.get_trait_items ())
    visit (item);
}

void
DefaultASTVisitor::visit (AST::InherentImpl &impl)
{
  visit_outer_attrs (impl);
  visit (impl.get_visibility ());

  for (auto &generic : impl.get_generic_params ())
    visit (generic);
  if (impl.has_where_clause ())
    visit (impl.get_where_clause ());
  visit (impl.get_type ());
  visit_inner_attrs (impl);
  for (auto &item : impl.get_impl_items ())
    visit (item);
}

void
DefaultASTVisitor::visit (AST::TraitImpl &impl)
{
  visit_outer_attrs (impl);
  visit (impl.get_visibility ());

  for (auto &generic : impl.get_generic_params ())
    visit (generic);
  if (impl.has_where_clause ())
    visit (impl.get_where_clause ());
  visit (impl.get_type ());
  visit (impl.get_trait_path ());
  visit_inner_attrs (impl);
  for (auto &item : impl.get_impl_items ())
    visit (item);
}

void
DefaultASTVisitor::visit (AST::ExternalTypeItem &item)
{
  visit_outer_attrs (item);
  visit (item.get_visibility ());
}

void
DefaultASTVisitor::visit (AST::ExternalStaticItem &item)
{
  visit_outer_attrs (item);
  visit (item.get_visibility ());
  visit (item.get_type ());
}

void
DefaultASTVisitor::visit (AST::ExternBlock &block)
{
  visit_outer_attrs (block);
  visit (block.get_visibility ());
  visit_inner_attrs (block);
  for (auto &item : block.get_extern_items ())
    visit (item);
}

void
DefaultASTVisitor::visit (AST::MacroMatchFragment &match)
{}

void
DefaultASTVisitor::visit (AST::MacroMatchRepetition &match)
{
  for (auto &m : match.get_matches ())
    visit (m);
}

void
DefaultASTVisitor::visit (AST::MacroMatcher &matcher)
{
  for (auto &m : matcher.get_matches ())
    visit (m);
}

void
DefaultASTVisitor::visit (AST::MacroTranscriber &transcriber)
{
  visit (transcriber.get_token_tree ());
}

void
DefaultASTVisitor::visit (AST::MacroRule &rule)
{
  visit (rule.get_matcher ());
  visit (rule.get_transcriber ());
}
void
DefaultASTVisitor::visit (AST::MacroRulesDefinition &rules_def)
{
  visit_outer_attrs (rules_def);
  for (auto &rule : rules_def.get_macro_rules ())
    visit (rule);
}

void
DefaultASTVisitor::visit (AST::MacroInvocData &data)
{
  visit (data.get_path ());
  visit (data.get_delim_tok_tree ());
}
void
DefaultASTVisitor::visit (AST::MacroInvocation &macro_invoc)
{
  visit_outer_attrs (macro_invoc);
  visit (macro_invoc.get_invoc_data ());
}

void
DefaultASTVisitor::visit (AST::MetaItemPath &meta_item)
{
  visit (meta_item.get_path ());
}

void
DefaultASTVisitor::visit (AST::MetaItemSeq &meta_item)
{
  visit (meta_item.get_path ());
  for (auto &inner : meta_item.get_seq ())
    visit (inner);
}

void
DefaultASTVisitor::visit (AST::MetaWord &meta_item)
{}

void
DefaultASTVisitor::visit (AST::MetaNameValueStr &meta_item)
{}

void
DefaultASTVisitor::visit (AST::MetaListPaths &meta_item)
{
  for (auto &path : meta_item.get_paths ())
    visit (path);
}

void
DefaultASTVisitor::visit (AST::MetaListNameValueStr &meta_item)
{
  for (auto &str : meta_item.get_values ())
    visit (str);
}

void
DefaultASTVisitor::visit (AST::LiteralPattern &pattern)
{}

void
DefaultASTVisitor::visit (AST::IdentifierPattern &pattern)
{
  if (pattern.has_pattern_to_bind ())
    visit (pattern.get_pattern_to_bind ());
}

void
DefaultASTVisitor::visit (AST::WildcardPattern &pattern)
{}

void
DefaultASTVisitor::visit (AST::RestPattern &pattern)
{}

void
DefaultASTVisitor::visit (AST::RangePatternBoundLiteral &bound)
{}

void
DefaultASTVisitor::visit (AST::RangePatternBoundPath &bound)
{
  visit (bound.get_path ());
}

void
DefaultASTVisitor::visit (AST::RangePatternBoundQualPath &bound)
{
  visit (bound.get_qualified_path ());
}

void
DefaultASTVisitor::visit (AST::RangePattern &pattern)
{
  if (pattern.get_has_lower_bound ())
    visit (pattern.get_lower_bound ());
  if (pattern.get_has_upper_bound ())
    visit (pattern.get_upper_bound ());
}

void
DefaultASTVisitor::visit (AST::ReferencePattern &pattern)
{
  visit (pattern.get_referenced_pattern ());
}

void
DefaultASTVisitor::visit (AST::StructPatternFieldTuplePat &field)
{
  visit_outer_attrs (field);
  visit (field.get_index_pattern ());
}

void
DefaultASTVisitor::visit (AST::StructPatternFieldIdentPat &field)
{
  visit_outer_attrs (field);
  visit (field.get_ident_pattern ());
}

void
DefaultASTVisitor::visit (AST::StructPatternFieldIdent &field)
{
  visit_outer_attrs (field);
}

void
DefaultASTVisitor::visit (AST::StructPatternElements &spe)
{
  for (auto &field : spe.get_struct_pattern_fields ())
    visit (field);
  for (auto &attribute : spe.get_etc_outer_attrs ())
    visit (attribute);
}

void
DefaultASTVisitor::visit (AST::StructPattern &pattern)
{
  visit (pattern.get_path ());
  visit (pattern.get_struct_pattern_elems ());
}

void
DefaultASTVisitor::visit (AST::TupleStructItemsNoRange &tuple_items)
{
  for (auto &pattern : tuple_items.get_patterns ())
    visit (pattern);
}

void
DefaultASTVisitor::visit (AST::TupleStructItemsRange &tuple_items)
{
  for (auto &lower : tuple_items.get_lower_patterns ())
    visit (lower);
  for (auto &upper : tuple_items.get_upper_patterns ())
    visit (upper);
}

void
DefaultASTVisitor::visit (AST::TupleStructPattern &pattern)
{
  visit (pattern.get_path ());
  visit (pattern.get_items ());
}

void
DefaultASTVisitor::visit (AST::TuplePatternItemsMultiple &tuple_items)
{
  for (auto &pattern : tuple_items.get_patterns ())
    visit (pattern);
}

void
DefaultASTVisitor::visit (AST::TuplePatternItemsRanged &tuple_items)
{
  for (auto &lower : tuple_items.get_lower_patterns ())
    visit (lower);
  for (auto &upper : tuple_items.get_upper_patterns ())
    visit (upper);
}

void
DefaultASTVisitor::visit (AST::TuplePattern &pattern)
{
  visit (pattern.get_items ());
}

void
DefaultASTVisitor::visit (AST::GroupedPattern &pattern)
{
  visit (pattern.get_pattern_in_parens ());
}

void
DefaultASTVisitor::visit (AST::SlicePattern &pattern)
{
  for (auto &item : pattern.get_items ())
    visit (item);
}

void
DefaultASTVisitor::visit (AST::AltPattern &pattern)
{
  for (auto &alt : pattern.get_alts ())
    visit (alt);
}

void
DefaultASTVisitor::visit (AST::EmptyStmt &stmt)
{}

void
DefaultASTVisitor::visit (AST::LetStmt &stmt)
{
  visit_outer_attrs (stmt);
  visit (stmt.get_pattern ());
  if (stmt.has_type ())
    visit (stmt.get_type ());
  if (stmt.has_init_expr ())
    visit (stmt.get_init_expr ());
}

void
DefaultASTVisitor::visit (AST::ExprStmt &stmt)
{
  visit (stmt.get_expr ());
}

void
DefaultASTVisitor::visit (AST::TraitBound &bound)
{
  for (auto &lifetime : bound.get_for_lifetimes ())
    visit (lifetime);
  visit (bound.get_type_path ());
}

void
DefaultASTVisitor::visit (AST::ImplTraitType &type)
{
  for (auto &bound : type.get_type_param_bounds ())
    visit (bound);
}

void
DefaultASTVisitor::visit (AST::TraitObjectType &type)
{
  for (auto &bound : type.get_type_param_bounds ())
    visit (bound);
}

void
DefaultASTVisitor::visit (AST::ParenthesisedType &type)
{
  visit (type.get_type_in_parens ());
}

void
DefaultASTVisitor::visit (AST::ImplTraitTypeOneBound &type)
{
  visit (type.get_trait_bound ());
}

void
DefaultASTVisitor::visit (AST::TraitObjectTypeOneBound &type)
{
  visit (type.get_trait_bound ());
}

void
DefaultASTVisitor::visit (AST::TupleType &type)
{
  for (auto &elem : type.get_elems ())
    visit (elem);
}

void
DefaultASTVisitor::visit (AST::NeverType &type)
{}

void
DefaultASTVisitor::visit (AST::RawPointerType &type)
{
  visit (type.get_type_pointed_to ());
}

void
DefaultASTVisitor::visit (AST::ReferenceType &type)
{
  visit (type.get_lifetime ());
  visit (type.get_base_type ());
}

void
DefaultASTVisitor::visit (AST::ArrayType &type)
{
  visit (type.get_elem_type ());
  visit (type.get_size_expr ());
}

void
DefaultASTVisitor::visit (AST::SliceType &type)
{
  visit (type.get_elem_type ());
}

void
DefaultASTVisitor::visit (AST::InferredType &type)
{}

void
DefaultASTVisitor::visit (AST::MaybeNamedParam &param)
{
  visit_outer_attrs (param);
  visit (param.get_type ());
}

void
DefaultASTVisitor::visit (AST::BareFunctionType &type)
{
  for (auto &lifetime : type.get_for_lifetimes ())
    visit (lifetime);
  visit (type.get_function_qualifiers ());
  for (auto &param : type.get_function_params ())
    visit (param);
  if (type.is_variadic ())
    for (auto attr : type.get_variadic_attr ())
      visit (attr);
  if (type.has_return_type ())
    visit (type.get_return_type ());
}

void
DefaultASTVisitor::visit (AST::FormatArgs &)
{
  // FIXME: Do we have anything to do? any subnodes to visit? Probably, right?
}

void
DefaultASTVisitor::visit (AST::VariadicParam &param)
{
  if (param.has_pattern ())
    visit (param.get_pattern ());
}

void
ContextualASTVisitor::visit (AST::Crate &crate)
{
  ctx.enter (Kind::CRATE);
  DefaultASTVisitor::visit (crate);
  ctx.exit ();
}

void
ContextualASTVisitor::visit (AST::InherentImpl &impl)
{
  ctx.enter (Kind::INHERENT_IMPL);
  DefaultASTVisitor::visit (impl);
  ctx.exit ();
}

void
ContextualASTVisitor::visit (AST::TraitImpl &impl)
{
  ctx.enter (Kind::TRAIT_IMPL);
  DefaultASTVisitor::visit (impl);
  ctx.exit ();
}

void
ContextualASTVisitor::visit (AST::Trait &trait)
{
  ctx.enter (Kind::TRAIT);
  DefaultASTVisitor::visit (trait);
  ctx.exit ();
}

} // namespace AST
} // namespace Rust
