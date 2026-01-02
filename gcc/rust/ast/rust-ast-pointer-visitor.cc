// Copyright (C) 2025-2026 Free Software Foundation, Inc.

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

#include "rust-ast-pointer-visitor.h"
#include "rust-ast-visitor.h"
#include "rust-ast-full.h"

namespace Rust {
namespace AST {

void
PointerVisitor::visit (AST::Crate &crate)
{
  visit_inner_attrs (crate);
  for (auto &item : crate.items)
    reseat (item);
}

void
PointerVisitor::visit (AST::AttrInputMetaItemContainer &input)
{
  // FIXME: I think we might actually have to reseat sub-items for macro
  // invocations within attributes correct?

  for (auto &item : input.get_items ())
    visit (item);
}

void
PointerVisitor::visit (AST::IdentifierExpr &ident_expr)
{
  visit_outer_attrs (ident_expr);
}

void
PointerVisitor::visit (AST::LifetimeParam &lifetime_param)
{
  visit_outer_attrs (lifetime_param);

  // Nothing to do for lifetimes right?
}

void
PointerVisitor::visit (AST::ConstGenericParam &const_param)
{
  visit_outer_attrs (const_param);
  if (const_param.has_type ())
    reseat (const_param.get_type_ptr ());

  if (const_param.has_default_value ())
    visit (const_param.get_default_value_unchecked ());
}

void
PointerVisitor::visit (AST::PathInExpression &path)
{
  visit_outer_attrs (path);

  if (!path.is_lang_item ())
    for (auto &segment : path.get_segments ())
      visit (segment);
}

void
PointerVisitor::visit (GenericArgsBinding &binding)
{
  reseat (binding.get_type_ptr ());
}

void
PointerVisitor::visit (AST::TypePathSegmentGeneric &segment)
{
  if (segment.has_generic_args ())
    visit (segment.get_generic_args ());
}

void
PointerVisitor::visit (AST::TypePathFunction &tpf)
{
  for (auto &input : tpf.get_params ())
    reseat (input);
  if (tpf.has_return_type ())
    reseat (tpf.get_return_type_ptr ());
}

void
PointerVisitor::visit (AST::TypePathSegmentFunction &segment)
{
  // FIXME: No reseating here correct? No macros possible or desugar?
  visit (segment.get_type_path_function ());
  visit (segment.get_ident_segment ());
}

void
PointerVisitor::visit (AST::GenericArgs &args)
{
  // Nothing to do for lifetimes?
  // for (auto &lifetime : args.get_lifetime_args ())
  //   reseat (lifetime);

  // FIXME: Actually this can probably be a macro invocation, so we need to
  // reseat them?
  for (auto &generic : args.get_generic_args ())
    visit (generic);

  for (auto &binding : args.get_binding_args ())
    visit (binding);
}

void
PointerVisitor::visit (AST::PathExprSegment &segment)
{
  visit (segment.get_ident_segment ());
  if (segment.has_generic_args ())
    visit (segment.get_generic_args ());
}
void
PointerVisitor::visit (AST::TypePath &path)
{
  for (auto &segment : path.get_segments ())
    visit (segment);
}

void
PointerVisitor::visit (AST::QualifiedPathInExpression &path)
{
  visit_outer_attrs (path);
  visit (path.get_qualified_path_type ());

  for (auto &segment : path.get_segments ())
    visit (segment);
}

void
PointerVisitor::visit (AST::QualifiedPathType &path)
{
  reseat (path.get_type_ptr ());
  if (path.has_as_clause ())
    visit (path.get_as_type_path ());
}

void
PointerVisitor::visit (AST::QualifiedPathInType &path)
{
  visit (path.get_qualified_path_type ());
  visit (path.get_associated_segment ());

  for (auto &segment : path.get_segments ())
    visit (segment);
}

void
PointerVisitor::visit (AST::LiteralExpr &expr)
{
  visit_outer_attrs (expr);
}

void
PointerVisitor::visit (AST::AttrInputLiteral &attr_input)
{
  visit (attr_input.get_literal ());
}

void
PointerVisitor::visit (AST::AttrInputMacro &attr_input)
{
  visit (attr_input.get_macro ());
}

void
PointerVisitor::visit (AST::MetaItemLitExpr &meta_item)
{
  visit (meta_item.get_literal ());
}

void
PointerVisitor::visit (AST::SimplePath &path)
{
  for (auto &segment : path.get_segments ())
    visit (segment);
}

void
PointerVisitor::visit (AST::MetaItemPathExpr &meta_item)
{
  visit (meta_item.get_path ());
  reseat (meta_item.get_expr_ptr ());
}

void
PointerVisitor::visit (AST::BorrowExpr &expr)
{
  visit_outer_attrs (expr);
  reseat (expr.get_borrowed_expr_ptr ());
}

void
PointerVisitor::visit (AST::DereferenceExpr &expr)
{
  visit_outer_attrs (expr);
  reseat (expr.get_dereferenced_expr_ptr ());
}

void
PointerVisitor::visit (AST::ErrorPropagationExpr &expr)
{
  visit_outer_attrs (expr);
  reseat (expr.get_propagating_expr_ptr ());
}

void
PointerVisitor::visit (AST::NegationExpr &expr)
{
  visit_outer_attrs (expr);
  reseat (expr.get_negated_expr_ptr ());
}

void
PointerVisitor::visit (AST::ArithmeticOrLogicalExpr &expr)
{
  visit_outer_attrs (expr);
  reseat (expr.get_left_expr_ptr ());
  reseat (expr.get_right_expr_ptr ());
}

void
PointerVisitor::visit (AST::ComparisonExpr &expr)
{
  visit_outer_attrs (expr);
  reseat (expr.get_left_expr_ptr ());
  reseat (expr.get_right_expr_ptr ());
}

void
PointerVisitor::visit (AST::LazyBooleanExpr &expr)
{
  visit_outer_attrs (expr);
  reseat (expr.get_left_expr_ptr ());
  reseat (expr.get_right_expr_ptr ());
}

void
PointerVisitor::visit (AST::TypeCastExpr &expr)
{
  visit_outer_attrs (expr);
  reseat (expr.get_casted_expr_ptr ());
  reseat (expr.get_type_to_cast_to_ptr ());
}

void
PointerVisitor::visit (AST::AssignmentExpr &expr)
{
  visit_outer_attrs (expr);
  reseat (expr.get_left_expr_ptr ());
  reseat (expr.get_right_expr_ptr ());
}

void
PointerVisitor::visit (AST::CompoundAssignmentExpr &expr)
{
  visit_outer_attrs (expr);
  reseat (expr.get_left_expr_ptr ());
  reseat (expr.get_right_expr_ptr ());
}

void
PointerVisitor::visit (AST::GroupedExpr &expr)
{
  visit_outer_attrs (expr);
  visit_inner_attrs (expr);
  reseat (expr.get_expr_in_parens_ptr ());
}

void
PointerVisitor::visit (AST::ArrayElemsValues &elems)
{
  for (auto &value : elems.get_values ())
    reseat (value);
}

void
PointerVisitor::visit (AST::ArrayElemsCopied &elems)
{
  reseat (elems.get_elem_to_copy_ptr ());
  reseat (elems.get_num_copies_ptr ());
}

void
PointerVisitor::visit (AST::ArrayExpr &expr)
{
  visit_outer_attrs (expr);
  visit_inner_attrs (expr);
  visit (expr.get_array_elems ());
}

void
PointerVisitor::visit (AST::ArrayIndexExpr &expr)
{
  visit_outer_attrs (expr);
  reseat (expr.get_array_expr_ptr ());
  reseat (expr.get_index_expr_ptr ());
}

void
PointerVisitor::visit (AST::TupleExpr &expr)
{
  visit_outer_attrs (expr);
  visit_inner_attrs (expr);
  for (auto &elem : expr.get_tuple_elems ())
    reseat (elem);
}

void
PointerVisitor::visit (AST::TupleIndexExpr &expr)
{
  visit_outer_attrs (expr);
  reseat (expr.get_tuple_expr_ptr ());
}

void
PointerVisitor::visit (AST::StructExprStruct &expr)
{
  visit_outer_attrs (expr);
  visit_inner_attrs (expr);
  visit (expr.get_struct_name ());
}

void
PointerVisitor::visit (AST::StructExprFieldIdentifier &field)
{}

void
PointerVisitor::visit (AST::StructExprFieldIdentifierValue &field)
{
  reseat (field.get_value_ptr ());
}

void
PointerVisitor::visit (AST::StructExprFieldIndexValue &field)
{
  reseat (field.get_value_ptr ());
}

void
PointerVisitor::visit (AST::StructBase &base)
{
  reseat (base.get_base_struct_ptr ());
}

void
PointerVisitor::visit (AST::StructExprStructFields &expr)
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
PointerVisitor::visit (AST::StructExprStructBase &expr)
{
  visit_outer_attrs (expr);
  visit_inner_attrs (expr);
  visit (expr.get_struct_name ());
  visit (expr.get_struct_base ());
}

void
PointerVisitor::visit (AST::CallExpr &expr)
{
  visit_outer_attrs (expr);
  reseat (expr.get_function_expr_ptr ());
  for (auto &param : expr.get_params ())
    reseat (param);
}

void
PointerVisitor::visit (AST::MethodCallExpr &expr)
{
  visit_outer_attrs (expr);
  reseat (expr.get_receiver_expr_ptr ());
  visit (expr.get_method_name ());
  for (auto &param : expr.get_params ())
    reseat (param);
}

void
PointerVisitor::visit (AST::FieldAccessExpr &expr)
{
  visit_outer_attrs (expr);
  reseat (expr.get_receiver_expr_ptr ());
}

void
PointerVisitor::visit (AST::ClosureExprInner &expr)
{
  visit_outer_attrs (expr);

  // TODO: Actually we need to handle macro invocations as closure parameters so
  // this needs to be a reseat
  for (auto &param : expr.get_params ())
    visit (param);

  reseat (expr.get_definition_expr_ptr ());
}

void
PointerVisitor::visit (AST::BlockExpr &expr)
{
  visit_outer_attrs (expr);
  visit_inner_attrs (expr);

  if (expr.has_label ())
    visit (expr.get_label ());

  for (auto &stmt : expr.get_statements ())
    reseat (stmt);

  if (expr.has_tail_expr ())
    reseat (expr.get_tail_expr_ptr ());
}

void
PointerVisitor::visit (AST::ConstBlock &expr)
{
  visit (expr.get_const_expr ());
}

void
PointerVisitor::visit (AST::AnonConst &expr)
{
  if (!expr.is_deferred ())
    reseat (expr.get_inner_expr_ptr ());
}

void
PointerVisitor::visit (AST::ClosureExprInnerTyped &expr)
{
  visit_outer_attrs (expr);

  // TODO: Same as ClosureExprInner
  for (auto &param : expr.get_params ())
    visit (param);

  reseat (expr.get_return_type_ptr ());

  reseat (expr.get_definition_expr_ptr ());
}

void
PointerVisitor::visit (AST::ClosureParam &param)
{
  visit_outer_attrs (param);
  reseat (param.get_pattern_ptr ());
  if (param.has_type_given ())
    reseat (param.get_type_ptr ());
}

void
PointerVisitor::visit (AST::ContinueExpr &expr)
{
  visit_outer_attrs (expr);
  if (expr.has_label ())
    visit (expr.get_label_unchecked ());
}

void
PointerVisitor::visit (AST::BreakExpr &expr)
{
  visit_outer_attrs (expr);
  if (expr.has_label ())
    visit (expr.get_label_unchecked ());

  if (expr.has_break_expr ())
    reseat (expr.get_break_expr_ptr ());
}

void
PointerVisitor::visit (AST::RangeFromToExpr &expr)
{
  reseat (expr.get_from_expr_ptr ());
  reseat (expr.get_to_expr_ptr ());
}

void
PointerVisitor::visit (AST::RangeFromExpr &expr)
{
  reseat (expr.get_from_expr_ptr ());
}

void
PointerVisitor::visit (AST::RangeToExpr &expr)
{
  reseat (expr.get_to_expr_ptr ());
}

void
PointerVisitor::visit (AST::RangeFullExpr &expr)
{}

void
PointerVisitor::visit (AST::RangeFromToInclExpr &expr)
{
  reseat (expr.get_from_expr_ptr ());
  reseat (expr.get_to_expr_ptr ());
}

void
PointerVisitor::visit (AST::RangeToInclExpr &expr)
{
  reseat (expr.get_to_expr_ptr ());
}

void
PointerVisitor::visit (AST::ReturnExpr &expr)
{
  visit_outer_attrs (expr);
  if (expr.has_returned_expr ())
    reseat (expr.get_returned_expr_ptr ());
}

void
PointerVisitor::visit (AST::TryExpr &expr)
{
  visit_outer_attrs (expr);
  reseat (expr.get_block_expr_ptr ());
}

void
PointerVisitor::visit (AST::BoxExpr &expr)
{
  visit_outer_attrs (expr);
  reseat (expr.get_boxed_expr_ptr ());
}

void
PointerVisitor::visit (AST::UnsafeBlockExpr &expr)
{
  visit_outer_attrs (expr);
  reseat (expr.get_block_expr_ptr ());
}

void
PointerVisitor::visit (AST::LoopLabel &label)
{
  visit (label.get_lifetime ());
}

void
PointerVisitor::visit (AST::LoopExpr &expr)
{
  visit_outer_attrs (expr);
  if (expr.has_loop_label ())
    visit (expr.get_loop_label ());
  reseat (expr.get_loop_block_ptr ());
}

void
PointerVisitor::visit (AST::WhileLoopExpr &expr)
{
  visit_outer_attrs (expr);
  if (expr.has_loop_label ())
    visit (expr.get_loop_label ());
  reseat (expr.get_predicate_expr_ptr ());
  reseat (expr.get_loop_block_ptr ());
}

void
PointerVisitor::visit (AST::WhileLetLoopExpr &expr)
{
  visit_outer_attrs (expr);
  for (auto &pattern : expr.get_patterns ())
    reseat (pattern);

  if (expr.has_loop_label ())
    visit (expr.get_loop_label ());

  reseat (expr.get_scrutinee_expr_ptr ());
  reseat (expr.get_loop_block_ptr ());
}

void
PointerVisitor::visit (AST::ForLoopExpr &expr)
{
  visit_outer_attrs (expr);
  reseat (expr.get_pattern_ptr ());
  reseat (expr.get_iterator_expr_ptr ());
  if (expr.has_loop_label ())
    visit (expr.get_loop_label ());
  reseat (expr.get_loop_block_ptr ());
}

void
PointerVisitor::visit (AST::IfExpr &expr)
{
  visit_outer_attrs (expr);
  reseat (expr.get_condition_expr_ptr ());
  visit (expr.get_if_block ());
}

void
PointerVisitor::visit (AST::IfExprConseqElse &expr)
{
  visit (reinterpret_cast<AST::IfExpr &> (expr));
  visit (expr.get_else_block ());
}

void
PointerVisitor::visit (AST::IfLetExpr &expr)
{
  visit_outer_attrs (expr);
  for (auto &pattern : expr.get_patterns ())
    reseat (pattern);
  reseat (expr.get_value_expr_ptr ());
  visit (expr.get_if_block ());
}

void
PointerVisitor::visit (AST::IfLetExprConseqElse &expr)
{
  visit (reinterpret_cast<AST::IfLetExpr &> (expr));
  visit (expr.get_else_block ());
}

void
PointerVisitor::visit (AST::MatchArm &arm)
{
  visit_outer_attrs (arm);
  for (auto &pattern : arm.get_patterns ())
    reseat (pattern);
  if (arm.has_match_arm_guard ())
    reseat (arm.get_guard_expr_ptr ());
}

void
PointerVisitor::visit (AST::MatchCase &arm)
{
  visit (arm.get_arm ());
  reseat (arm.get_expr_ptr ());
}

void
PointerVisitor::visit (AST::MatchExpr &expr)
{
  visit_outer_attrs (expr);
  visit_inner_attrs (expr);
  reseat (expr.get_scrutinee_expr_ptr ());
  for (auto &arm : expr.get_match_cases ())
    visit (arm);
}

void
PointerVisitor::visit (AST::AwaitExpr &expr)
{
  visit_outer_attrs (expr);
  reseat (expr.get_awaited_expr ());
}

void
PointerVisitor::visit (AST::AsyncBlockExpr &expr)
{
  visit_outer_attrs (expr);
  reseat (expr.get_block_expr ());
}

void
PointerVisitor::visit (AST::InlineAsm &expr)
{
  visit_outer_attrs (expr);
  using RegisterType = AST::InlineAsmOperand::RegisterType;
  for (auto &operand : expr.get_operands ())
    {
      switch (operand.get_register_type ())
	{
	case RegisterType::In:
	  {
	    reseat (operand.get_in ().expr);
	    break;
	  }
	case RegisterType::Out:
	  {
	    reseat (operand.get_out ().expr);
	    break;
	  }
	case RegisterType::InOut:
	  {
	    reseat (operand.get_in_out ().expr);
	    break;
	  }
	case RegisterType::SplitInOut:
	  {
	    auto split = operand.get_split_in_out ();
	    reseat (split.in_expr);
	    reseat (split.out_expr);
	    break;
	  }
	case RegisterType::Const:
	  {
	    reseat (operand.get_const ().anon_const.get_inner_expr_ptr ());
	    break;
	  }
	case RegisterType::Sym:
	  {
	    reseat (operand.get_sym ().expr);
	    break;
	  }
	case RegisterType::Label:
	  {
	    reseat (operand.get_label ().expr);
	    break;
	  }
	}
    }
}

void
PointerVisitor::visit (AST::LlvmInlineAsm &expr)
{
  for (auto &output : expr.get_outputs ())
    reseat (output.expr);

  for (auto &input : expr.get_inputs ())
    reseat (input.expr);
}

void
PointerVisitor::visit (AST::TypeParam &param)
{
  visit_outer_attrs (param);
  // FIXME: Can we do macro expansion here?
  for (auto &bound : param.get_type_param_bounds ())
    visit (bound);
  if (param.has_type ())
    reseat (param.get_type_ptr ());
}

void
PointerVisitor::visit (AST::LifetimeWhereClauseItem &item)
{
  visit (item.get_lifetime ());
  for (auto &bound : item.get_lifetime_bounds ())
    visit (bound);
}

void
PointerVisitor::visit (AST::TypeBoundWhereClauseItem &item)
{
  for (auto &lifetime : item.get_for_lifetimes ())
    visit (lifetime);
  reseat (item.get_type_ptr ());
  // FIXME: Likewise?
  for (auto &param : item.get_type_param_bounds ())
    visit (param);
}

void
PointerVisitor::visit (AST::Visibility &vis)
{
  if (vis.has_path ())
    visit (vis.get_path ());
}

void
PointerVisitor::visit (AST::WhereClause &where)
{
  for (auto &item : where.get_items ())
    visit (item);
}

void
PointerVisitor::visit (AST::FunctionParam &param)
{
  visit_outer_attrs (param);
  if (param.has_name ())
    reseat (param.get_pattern_ptr ());

  reseat (param.get_type_ptr ());
}

void
PointerVisitor::visit (AST::SelfParam &param)
{
  visit_outer_attrs (param);

  if (param.has_lifetime ())
    visit (param.get_lifetime ());

  if (param.has_type ())
    reseat (param.get_type_ptr ());
}

void
PointerVisitor::visit (AST::Module &module)
{
  visit_outer_attrs (module);
  visit (module.get_visibility ());
  visit_inner_attrs (module);
  for (auto &item : module.get_items ())
    reseat (item);
}

void
PointerVisitor::visit (AST::ExternCrate &crate)
{
  visit_outer_attrs (crate);
  visit (crate.get_visibility ());
}

void
PointerVisitor::visit (AST::UseTreeGlob &use_tree)
{
  visit (use_tree.get_path ());
}

void
PointerVisitor::visit (AST::UseTreeList &use_tree)
{
  visit (use_tree.get_path ());
}

void
PointerVisitor::visit (AST::UseTreeRebind &use_tree)
{
  visit (use_tree.get_path ());
}

void
PointerVisitor::visit (AST::UseDeclaration &use_decl)
{
  visit (use_decl.get_visibility ());
  visit (use_decl.get_tree ());
}

void
PointerVisitor::visit_function_params (AST::Function &function)
{
  for (auto &param : function.get_function_params ())
    visit (param);
}

void
PointerVisitor::visit (AST::Function &function)
{
  visit_outer_attrs (function);
  visit (function.get_visibility ());
  visit (function.get_qualifiers ());
  for (auto &generic : function.get_generic_params ())
    visit (generic);

  visit_function_params (function);

  if (function.has_return_type ())
    reseat (function.get_return_type_ptr ());
  if (function.has_where_clause ())
    visit (function.get_where_clause ());
  if (function.has_body ())
    reseat (*function.get_definition ());
}

void
PointerVisitor::visit (AST::TypeAlias &type_alias)
{
  visit_outer_attrs (type_alias);
  visit (type_alias.get_visibility ());
  for (auto &generic : type_alias.get_generic_params ())
    visit (generic);
  if (type_alias.has_where_clause ())
    visit (type_alias.get_where_clause ());
  reseat (type_alias.get_type_aliased_ptr ());
}

void
PointerVisitor::visit (AST::StructField &field)
{
  visit_outer_attrs (field);
  visit (field.get_visibility ());
  reseat (field.get_field_type_ptr ());
}

void
PointerVisitor::visit (AST::StructStruct &struct_item)
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
PointerVisitor::visit (AST::TupleField &field)
{
  visit_outer_attrs (field);
  visit (field.get_visibility ());
  reseat (field.get_field_type_ptr ());
}

void
PointerVisitor::visit (AST::TupleStruct &tuple_struct)
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
PointerVisitor::visit (AST::EnumItem &item)
{
  visit_outer_attrs (item);
  visit (item.get_visibility ());
}

void
PointerVisitor::visit (AST::EnumItemTuple &item)
{
  PointerVisitor::visit (static_cast<EnumItem &> (item));
  for (auto &field : item.get_tuple_fields ())
    visit (field);
}

void
PointerVisitor::visit (AST::EnumItemStruct &item)
{
  PointerVisitor::visit (static_cast<EnumItem &> (item));
  for (auto &field : item.get_struct_fields ())
    visit (field);
}

void
PointerVisitor::visit (AST::EnumItemDiscriminant &item)
{
  PointerVisitor::visit (static_cast<EnumItem &> (item));
  reseat (item.get_expr_ptr ());
}

void
PointerVisitor::visit (AST::Enum &enum_item)
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
PointerVisitor::visit (AST::Union &union_item)
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
PointerVisitor::visit (AST::ConstantItem &const_item)
{
  visit_outer_attrs (const_item);
  visit (const_item.get_visibility ());
  reseat (const_item.get_type_ptr ());
  if (const_item.has_expr ())
    reseat (const_item.get_expr_ptr ());
}

void
PointerVisitor::visit (AST::StaticItem &static_item)
{
  visit_outer_attrs (static_item);
  visit (static_item.get_visibility ());
  reseat (static_item.get_type_ptr ());
  reseat (static_item.get_expr_ptr ());
}

void
PointerVisitor::visit (AST::TraitItemType &item)
{
  visit_outer_attrs (item);
  for (auto &bound : item.get_type_param_bounds ())
    visit (bound);
}

void
PointerVisitor::visit (AST::Trait &trait)
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
    reseat (item);
}

void
PointerVisitor::visit (AST::InherentImpl &impl)
{
  visit_outer_attrs (impl);
  visit (impl.get_visibility ());

  for (auto &generic : impl.get_generic_params ())
    visit (generic);
  if (impl.has_where_clause ())
    visit (impl.get_where_clause ());
  reseat (impl.get_type_ptr ());
  visit_inner_attrs (impl);
  for (auto &item : impl.get_impl_items ())
    reseat (item);
}

void
PointerVisitor::visit (AST::TraitImpl &impl)
{
  visit_outer_attrs (impl);
  visit (impl.get_visibility ());

  for (auto &generic : impl.get_generic_params ())
    visit (generic);
  if (impl.has_where_clause ())
    visit (impl.get_where_clause ());
  reseat (impl.get_type_ptr ());
  visit (impl.get_trait_path ());
  visit_inner_attrs (impl);
  for (auto &item : impl.get_impl_items ())
    reseat (item);
}

void
PointerVisitor::visit (AST::ExternalTypeItem &item)
{
  visit_outer_attrs (item);
  visit (item.get_visibility ());
}

void
PointerVisitor::visit (AST::ExternalStaticItem &item)
{
  visit_outer_attrs (item);
  visit (item.get_visibility ());
  reseat (item.get_type_ptr ());
}

void
PointerVisitor::visit (AST::ExternBlock &block)
{
  visit_outer_attrs (block);
  visit (block.get_visibility ());
  visit_inner_attrs (block);
  for (auto &item : block.get_extern_items ())
    reseat (item);
}

void
PointerVisitor::visit (AST::MacroMatchFragment &match)
{}

void
PointerVisitor::visit (AST::MacroMatchRepetition &match)
{
  for (auto &m : match.get_matches ())
    visit (m);
}

void
PointerVisitor::visit (AST::MacroMatcher &matcher)
{
  for (auto &m : matcher.get_matches ())
    visit (m);
}

void
PointerVisitor::visit (AST::MacroTranscriber &transcriber)
{
  visit (transcriber.get_token_tree ());
}

void
PointerVisitor::visit (AST::MacroRule &rule)
{
  visit (rule.get_matcher ());
  visit (rule.get_transcriber ());
}

void
PointerVisitor::visit (AST::MacroRulesDefinition &rules_def)
{
  visit_outer_attrs (rules_def);
  for (auto &rule : rules_def.get_macro_rules ())
    visit (rule);
}

void
PointerVisitor::visit (AST::MacroInvocData &data)
{
  visit (data.get_path ());
  visit (data.get_delim_tok_tree ());
}

void
PointerVisitor::visit (AST::MacroInvocation &macro_invoc)
{
  visit_outer_attrs (macro_invoc);
  visit (macro_invoc.get_invoc_data ());
}

void
PointerVisitor::visit (AST::MetaItemPath &meta_item)
{
  visit (meta_item.get_path ());
}

void
PointerVisitor::visit (AST::MetaItemSeq &meta_item)
{
  visit (meta_item.get_path ());
  for (auto &inner : meta_item.get_seq ())
    visit (inner);
}

void
PointerVisitor::visit (AST::MetaListPaths &meta_item)
{
  for (auto &path : meta_item.get_paths ())
    visit (path);
}

void
PointerVisitor::visit (AST::MetaListNameValueStr &meta_item)
{
  for (auto &str : meta_item.get_values ())
    visit (str);
}

void
PointerVisitor::visit (AST::IdentifierPattern &pattern)
{
  if (pattern.has_subpattern ())
    reseat (pattern.get_subpattern_ptr ());
}

void
PointerVisitor::visit (AST::RangePatternBoundPath &bound)
{
  visit (bound.get_path ());
}

void
PointerVisitor::visit (AST::RangePatternBoundQualPath &bound)
{
  visit (bound.get_qualified_path ());
}

void
PointerVisitor::visit (AST::RangePattern &pattern)
{
  // FIXME: So should this be reseat() instead? Can we have macro invocations as
  // patterns in range patterns?
  if (pattern.get_has_lower_bound ())
    visit (pattern.get_lower_bound ());
  if (pattern.get_has_upper_bound ())
    visit (pattern.get_upper_bound ());
}

void
PointerVisitor::visit (AST::ReferencePattern &pattern)
{
  reseat (pattern.get_referenced_pattern_ptr ());
}

void
PointerVisitor::visit (AST::StructPatternFieldTuplePat &field)
{
  visit_outer_attrs (field);
  reseat (field.get_index_pattern_ptr ());
}

void
PointerVisitor::visit (AST::StructPatternFieldIdentPat &field)
{
  visit_outer_attrs (field);
  reseat (field.get_ident_pattern_ptr ());
}

void
PointerVisitor::visit (AST::StructPatternFieldIdent &field)
{
  visit_outer_attrs (field);
}

void
PointerVisitor::visit (AST::StructPatternElements &spe)
{
  for (auto &field : spe.get_struct_pattern_fields ())
    visit (field);
  for (auto &attribute : spe.get_etc_outer_attrs ())
    visit (attribute);
}

void
PointerVisitor::visit (AST::StructPattern &pattern)
{
  visit (pattern.get_path ());
  visit (pattern.get_struct_pattern_elems ());
}

void
PointerVisitor::visit (AST::TupleStructItemsNoRest &tuple_items)
{
  for (auto &pattern : tuple_items.get_patterns ())
    reseat (pattern);
}

void
PointerVisitor::visit (AST::TupleStructItemsHasRest &tuple_items)
{
  for (auto &lower : tuple_items.get_lower_patterns ())
    reseat (lower);
  for (auto &upper : tuple_items.get_upper_patterns ())
    reseat (upper);
}

void
PointerVisitor::visit (AST::TupleStructPattern &pattern)
{
  visit (pattern.get_path ());
  visit (pattern.get_items ());
}

void
PointerVisitor::visit (AST::TuplePatternItemsNoRest &tuple_items)
{
  for (auto &pattern : tuple_items.get_patterns ())
    reseat (pattern);
}

void
PointerVisitor::visit (AST::TuplePatternItemsHasRest &tuple_items)
{
  for (auto &lower : tuple_items.get_lower_patterns ())
    reseat (lower);
  for (auto &upper : tuple_items.get_upper_patterns ())
    reseat (upper);
}

void
PointerVisitor::visit (AST::TuplePattern &pattern)
{
  visit (pattern.get_items ());
}

void
PointerVisitor::visit (AST::GroupedPattern &pattern)
{
  reseat (pattern.get_pattern_in_parens_ptr ());
}

void
PointerVisitor::visit (AST::SlicePatternItemsNoRest &items)
{
  for (auto &item : items.get_patterns ())
    reseat (item);
}

void
PointerVisitor::visit (AST::SlicePatternItemsHasRest &items)
{
  for (auto &item : items.get_lower_patterns ())
    reseat (item);
  for (auto &item : items.get_upper_patterns ())
    reseat (item);
}

void
PointerVisitor::visit (AST::SlicePattern &pattern)
{
  visit (pattern.get_items ());
}

void
PointerVisitor::visit (AST::AltPattern &pattern)
{
  for (auto &alt : pattern.get_alts ())
    reseat (alt);
}

void
PointerVisitor::visit (AST::EmptyStmt &stmt)
{}

void
PointerVisitor::visit (AST::LetStmt &stmt)
{
  visit_outer_attrs (stmt);
  reseat (stmt.get_pattern_ptr ());
  if (stmt.has_type ())
    reseat (stmt.get_type_ptr ());
  if (stmt.has_init_expr ())
    reseat (stmt.get_init_expr_ptr ());
}

void
PointerVisitor::visit (AST::ExprStmt &stmt)
{
  reseat (stmt.get_expr_ptr ());
}

void
PointerVisitor::visit (AST::TraitBound &bound)
{
  for (auto &lifetime : bound.get_for_lifetimes ())
    visit (lifetime);
  visit (bound.get_type_path ());
}

void
PointerVisitor::visit (AST::ImplTraitType &type)
{
  for (auto &bound : type.get_type_param_bounds ())
    visit (bound);
}

void
PointerVisitor::visit (AST::TraitObjectType &type)
{
  for (auto &bound : type.get_type_param_bounds ())
    visit (bound);
}

void
PointerVisitor::visit (AST::ParenthesisedType &type)
{
  reseat (type.get_type_in_parens ());
}

void
PointerVisitor::visit (AST::ImplTraitTypeOneBound &type)
{
  // FIXME: Do we need to reseat here?
  visit (type.get_trait_bound ());
}

void
PointerVisitor::visit (AST::TraitObjectTypeOneBound &type)
{
  // FIXME: Do we need to reseat here?
  visit (type.get_trait_bound ());
}

void
PointerVisitor::visit (AST::TupleType &type)
{
  for (auto &elem : type.get_elems ())
    reseat (elem);
}

void
PointerVisitor::visit (AST::NeverType &type)
{}

void
PointerVisitor::visit (AST::RawPointerType &type)
{
  reseat (type.get_type_pointed_to_ptr ());
}

void
PointerVisitor::visit (AST::ReferenceType &type)
{
  visit (type.get_lifetime ());
  reseat (type.get_type_ptr ());
}

void
PointerVisitor::visit (AST::ArrayType &type)
{
  reseat (type.get_elem_type_ptr ());
  visit (type.get_size_expr ());
}

void
PointerVisitor::visit (AST::SliceType &type)
{
  reseat (type.get_elem_type_ptr ());
}

void
PointerVisitor::visit (AST::InferredType &type)
{}

void
PointerVisitor::visit (AST::MaybeNamedParam &param)
{
  visit_outer_attrs (param);
  reseat (param.get_type_ptr ());
}

void
PointerVisitor::visit (AST::BareFunctionType &type)
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
    reseat (type.get_return_type_ptr ());
}

void
PointerVisitor::visit (AST::FormatArgs &)
{
  // FIXME: Do we have anything to do? any subnodes to visit? Probably, right?
}

void
PointerVisitor::visit (AST::OffsetOf &offset_of)
{
  reseat (offset_of.get_type_ptr ());
}

void
PointerVisitor::visit (AST::VariadicParam &param)
{
  if (param.has_pattern ())
    reseat (param.get_pattern_ptr ());
}

} // namespace AST
} // namespace Rust
