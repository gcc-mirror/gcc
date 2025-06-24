// Copyright (C) 2021-2025 Free Software Foundation, Inc.

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

#include "rust-expr.h"
#include "rust-hir-full-decls.h"
#include "rust-hir-visitor.h"
#include "rust-hir-full.h"
#include "rust-system.h"

namespace Rust {
namespace HIR {

void
DefaultHIRVisitor::walk (Lifetime &)
{}

void
DefaultHIRVisitor::walk (LifetimeParam &lifetime_param)
{
  visit_outer_attrs (lifetime_param);
  lifetime_param.get_lifetime ().accept_vis (*this);
  for (Lifetime &lifetime_bound : lifetime_param.get_lifetime_bounds ())
    lifetime_bound.accept_vis (*this);
}

void
DefaultHIRVisitor::visit_generic_args (GenericArgs &generic_args)
{
  for (auto &lifetime : generic_args.get_lifetime_args ())
    lifetime.accept_vis (*this);
  for (auto &type : generic_args.get_type_args ())
    type->accept_vis (*this);
  for (auto &binding : generic_args.get_binding_args ())
    binding.get_type ().accept_vis (*this);
  for (auto &const_arg : generic_args.get_const_args ())
    const_arg.get_expression ()->accept_vis (*this);
}

void
DefaultHIRVisitor::walk (PathInExpression &path_in_expr)
{
  visit_outer_attrs (path_in_expr);
  if (!path_in_expr.is_lang_item ())
    for (auto &segment : path_in_expr.get_segments ())
      visit_path_expr_segment (segment);
}

void
DefaultHIRVisitor::walk (TypePathSegment &)
{}

void
DefaultHIRVisitor::walk (TypePathSegmentFunction &segment_function)
{
  TypePathFunction &function_path = segment_function.get_function_path ();
  if (function_path.has_inputs ())
    for (auto &param : function_path.get_params ())
      param->accept_vis (*this);
  if (function_path.has_return_type ())
    function_path.get_return_type ().accept_vis (*this);
}

void
DefaultHIRVisitor::walk (TypePathSegmentGeneric &segment_generic)
{
  if (segment_generic.has_generic_args ())
    visit_generic_args (segment_generic.get_generic_args ());
}

void
DefaultHIRVisitor::walk (TypePath &type_path)
{
  for (auto &segment : type_path.get_segments ())
    segment->accept_vis (*this);
}

void
DefaultHIRVisitor::visit_qualified_path_type (QualifiedPathType &path)
{
  path.get_type ().accept_vis (*this);
  if (path.has_as_clause ())
    path.get_trait ().accept_vis (*this);
}

// TODO: Implement visit_path_expr_segment
void
DefaultHIRVisitor::visit_path_expr_segment (PathExprSegment &segment)
{
  if (segment.has_generic_args ())
    visit_generic_args (segment.get_generic_args ());
}

void
DefaultHIRVisitor::walk (QualifiedPathInExpression &path_in_expr)
{
  visit_outer_attrs (path_in_expr);
  visit_qualified_path_type (path_in_expr.get_path_type ());
  for (auto &segment : path_in_expr.get_segments ())
    visit_path_expr_segment (segment);
}

void
DefaultHIRVisitor::walk (QualifiedPathInType &path_in_type)
{
  visit_qualified_path_type (path_in_type.get_path_type ());
  path_in_type.get_associated_segment ().accept_vis (*this);
  for (auto &segment : path_in_type.get_segments ())
    segment->accept_vis (*this);
}

void
DefaultHIRVisitor::walk (LiteralExpr &expr)
{
  visit_outer_attrs (expr);
}

void
DefaultHIRVisitor::walk (BorrowExpr &expr)
{
  visit_outer_attrs (expr);
  expr.get_expr ().accept_vis (*this);
}

void
DefaultHIRVisitor::walk (DereferenceExpr &expr)
{
  visit_outer_attrs (expr);
  expr.get_expr ().accept_vis (*this);
}

void
DefaultHIRVisitor::walk (ErrorPropagationExpr &expr)
{
  visit_outer_attrs (expr);
  expr.get_expr ().accept_vis (*this);
}

void
DefaultHIRVisitor::walk (NegationExpr &expr)
{
  visit_outer_attrs (expr);
  expr.get_expr ().accept_vis (*this);
}

void
DefaultHIRVisitor::walk (ArithmeticOrLogicalExpr &expr)
{
  visit_outer_attrs (expr);
  expr.get_lhs ().accept_vis (*this);
  expr.get_rhs ().accept_vis (*this);
}

void
DefaultHIRVisitor::walk (ComparisonExpr &expr)
{
  visit_outer_attrs (expr);
  expr.get_lhs ().accept_vis (*this);
  expr.get_rhs ().accept_vis (*this);
}

void
DefaultHIRVisitor::walk (LazyBooleanExpr &expr)
{
  visit_outer_attrs (expr);
  expr.get_lhs ().accept_vis (*this);
  expr.get_rhs ().accept_vis (*this);
}

void
DefaultHIRVisitor::walk (TypeCastExpr &expr)
{
  visit_outer_attrs (expr);
  expr.get_expr ().accept_vis (*this);
  expr.get_type_to_convert_to ().accept_vis (*this);
}

void
DefaultHIRVisitor::walk (AssignmentExpr &expr)
{
  visit_outer_attrs (expr);
  expr.get_lhs ().accept_vis (*this);
  expr.get_rhs ().accept_vis (*this);
}

void
DefaultHIRVisitor::walk (CompoundAssignmentExpr &expr)
{
  visit_outer_attrs (expr);
  expr.get_lhs ().accept_vis (*this);
  expr.get_rhs ().accept_vis (*this);
}

void
DefaultHIRVisitor::walk (GroupedExpr &expr)
{
  visit_outer_attrs (expr);
  visit_inner_attrs (expr);
  expr.get_expr_in_parens ().accept_vis (*this);
}

void
DefaultHIRVisitor::walk (ArrayElemsValues &elems)
{
  for (auto &elem : elems.get_values ())
    elem->accept_vis (*this);
}

void
DefaultHIRVisitor::walk (ArrayElemsCopied &elems)
{
  elems.get_elem_to_copy ().accept_vis (*this);
  elems.get_num_copies_expr ().accept_vis (*this);
}

void
DefaultHIRVisitor::walk (ArrayExpr &expr)
{
  visit_outer_attrs (expr);
  visit_inner_attrs (expr);
  expr.get_internal_elements ().accept_vis (*this);
}

void
DefaultHIRVisitor::walk (ArrayIndexExpr &expr)
{
  visit_outer_attrs (expr);
  expr.get_array_expr ().accept_vis (*this);
  expr.get_index_expr ().accept_vis (*this);
}

void
DefaultHIRVisitor::walk (TupleExpr &expr)
{
  visit_outer_attrs (expr);
  visit_inner_attrs (expr);
  for (auto &elem : expr.get_tuple_elems ())
    elem->accept_vis (*this);
}

void
DefaultHIRVisitor::walk (TupleIndexExpr &expr)
{
  visit_outer_attrs (expr);
  expr.get_tuple_expr ().accept_vis (*this);
}

void
DefaultHIRVisitor::walk (StructExprStruct &expr)
{
  visit_outer_attrs (expr);
  visit_inner_attrs (expr);
  expr.get_struct_name ().accept_vis (*this);
}

void
DefaultHIRVisitor::walk (StructExprFieldIdentifier &)
{}

void
DefaultHIRVisitor::walk (StructExprFieldIdentifierValue &field)
{
  field.get_value ().accept_vis (*this);
}

void
DefaultHIRVisitor::walk (StructExprFieldIndexValue &field)
{
  field.get_value ().accept_vis (*this);
}

void
DefaultHIRVisitor::walk (StructExprStructFields &expr)
{
  visit_outer_attrs (expr);
  visit_inner_attrs (expr);
  expr.get_struct_name ().accept_vis (*this);
  if (expr.has_struct_base ())
    {
      StructBase &base = expr.get_struct_base ();
      base.get_base ().accept_vis (*this);
    }
  for (auto &field : expr.get_fields ())
    field->accept_vis (*this);
}

void
DefaultHIRVisitor::walk (StructExprStructBase &expr)
{
  visit_outer_attrs (expr);
  visit_inner_attrs (expr);
  expr.get_struct_name ().accept_vis (*this);
  StructBase &base = expr.get_struct_base ();
  base.get_base ().accept_vis (*this);
}

void
DefaultHIRVisitor::walk (CallExpr &expr)
{
  visit_outer_attrs (expr);
  expr.get_fnexpr ().accept_vis (*this);
  for (auto &arg : expr.get_arguments ())
    arg->accept_vis (*this);
}

void
DefaultHIRVisitor::walk (MethodCallExpr &expr)
{
  visit_outer_attrs (expr);
  expr.get_receiver ().accept_vis (*this);
  visit_path_expr_segment (expr.get_method_name ());
  for (auto &arg : expr.get_arguments ())
    arg->accept_vis (*this);
}

void
DefaultHIRVisitor::walk (FieldAccessExpr &expr)
{
  visit_outer_attrs (expr);
  expr.get_receiver_expr ().accept_vis (*this);
}

void
DefaultHIRVisitor::visit_closure_param (ClosureParam &param)
{
  visit_outer_attrs (param);
  param.get_pattern ().accept_vis (*this);
  if (param.has_type_given ())
    {
      param.get_type ().accept_vis (*this);
    }
}

void
DefaultHIRVisitor::walk (ClosureExpr &expr)
{
  visit_outer_attrs (expr);
  for (auto &param : expr.get_params ())
    visit_closure_param (param);
  if (expr.has_return_type ())
    expr.get_return_type ().accept_vis (*this);
  expr.get_expr ().accept_vis (*this);
}

void
DefaultHIRVisitor::walk (BlockExpr &expr)
{
  visit_outer_attrs (expr);
  visit_inner_attrs (expr);
  for (auto &stmt : expr.get_statements ())
    stmt->accept_vis (*this);
  if (expr.has_expr ())
    expr.get_final_expr ().accept_vis (*this);
}

void
DefaultHIRVisitor::walk (AnonConst &expr)
{
  expr.get_inner_expr ().accept_vis (*this);
}

void
DefaultHIRVisitor::walk (ConstBlock &expr)
{
  expr.get_const_expr ().accept_vis (*this);
}

void
DefaultHIRVisitor::walk (ContinueExpr &expr)
{
  visit_outer_attrs (expr);
  if (expr.has_label ())
    expr.get_label ().accept_vis (*this);
}

void
DefaultHIRVisitor::walk (BreakExpr &expr)
{
  visit_outer_attrs (expr);
  if (expr.has_label ())
    expr.get_label ().accept_vis (*this);

  if (expr.has_break_expr ())
    expr.get_expr ().accept_vis (*this);
}

void
DefaultHIRVisitor::walk (RangeFromToExpr &expr)
{
  expr.get_from_expr ().accept_vis (*this);
  expr.get_to_expr ().accept_vis (*this);
}

void
DefaultHIRVisitor::walk (RangeFromExpr &expr)
{
  expr.get_from_expr ().accept_vis (*this);
}

void
DefaultHIRVisitor::walk (RangeToExpr &expr)
{
  expr.get_to_expr ().accept_vis (*this);
}

void
DefaultHIRVisitor::walk (RangeFullExpr &)
{}

void
DefaultHIRVisitor::walk (RangeFromToInclExpr &expr)
{
  expr.get_from_expr ().accept_vis (*this);
  expr.get_to_expr ().accept_vis (*this);
}

void
DefaultHIRVisitor::walk (RangeToInclExpr &expr)
{
  expr.get_to_expr ().accept_vis (*this);
}

void
DefaultHIRVisitor::walk (ReturnExpr &expr)
{
  visit_outer_attrs (expr);
  if (expr.has_return_expr ())
    expr.get_expr ().accept_vis (*this);
}

void
DefaultHIRVisitor::walk (UnsafeBlockExpr &expr)
{
  visit_outer_attrs (expr);
  expr.get_block_expr ().accept_vis (*this);
}

void
DefaultHIRVisitor::visit_loop_label (LoopLabel &label)
{
  label.get_lifetime ().accept_vis (*this);
}

void
DefaultHIRVisitor::walk (LoopExpr &expr)
{
  visit_outer_attrs (expr);
  if (expr.has_loop_label ())
    visit_loop_label (expr.get_loop_label ());
  expr.get_loop_block ().accept_vis (*this);
}

void
DefaultHIRVisitor::walk (WhileLoopExpr &expr)
{
  visit_outer_attrs (expr);
  if (expr.has_loop_label ())
    visit_loop_label (expr.get_loop_label ());
  expr.get_predicate_expr ().accept_vis (*this);
  expr.get_loop_block ().accept_vis (*this);
}

void
DefaultHIRVisitor::walk (WhileLetLoopExpr &expr)
{
  visit_outer_attrs (expr);
  for (auto &pattern : expr.get_patterns ())
    pattern->accept_vis (*this);
  if (expr.has_loop_label ())
    visit_loop_label (expr.get_loop_label ());
  expr.get_cond ().accept_vis (*this);
  expr.get_loop_block ().accept_vis (*this);
}

void
DefaultHIRVisitor::walk (IfExpr &expr)
{
  visit_outer_attrs (expr);
  expr.get_if_condition ().accept_vis (*this);
  expr.get_if_block ().accept_vis (*this);
}

void
DefaultHIRVisitor::walk (IfExprConseqElse &expr)
{
  reinterpret_cast<IfExpr &> (expr).accept_vis (*this);
  expr.get_else_block ().accept_vis (*this);
}

void
DefaultHIRVisitor::visit_match_arm (MatchArm &arm)
{
  // visit_outer_attrs (arm);
  for (auto &pattern : arm.get_patterns ())
    pattern->accept_vis (*this);
  if (arm.has_match_arm_guard ())
    arm.get_guard_expr ().accept_vis (*this);
}

void
DefaultHIRVisitor::visit_match_case (MatchCase &arm)
{
  visit_match_arm (arm.get_arm ());
  arm.get_expr ().accept_vis (*this);
}

void
DefaultHIRVisitor::walk (MatchExpr &expr)
{
  visit_outer_attrs (expr);
  visit_inner_attrs (expr);
  expr.get_scrutinee_expr ().accept_vis (*this);
  for (auto &arm : expr.get_match_cases ())
    visit_match_case (arm);
}

void
DefaultHIRVisitor::walk (AwaitExpr &expr)
{
  visit_outer_attrs (expr);
  expr.get_awaited_expr ().accept_vis (*this);
}

void
DefaultHIRVisitor::walk (AsyncBlockExpr &expr)
{
  visit_outer_attrs (expr);
  expr.get_block_expr ().accept_vis (*this);
}

void
DefaultHIRVisitor::walk (InlineAsm &expr)
{
  visit_outer_attrs (expr);
  const auto &operands = expr.get_operands ();
  using RegisterType = AST::InlineAsmOperand::RegisterType;
  for (auto &operand : operands)
    {
      switch (operand.get_register_type ())
	{
	case RegisterType::In:
	  {
	    operand.get_in ().expr->accept_vis (*this);
	    break;
	  }
	case RegisterType::Out:
	  {
	    operand.get_out ().expr->accept_vis (*this);
	    break;
	  }
	case RegisterType::InOut:
	  {
	    operand.get_in_out ().expr->accept_vis (*this);
	    break;
	  }
	case RegisterType::SplitInOut:
	  {
	    operand.get_split_in_out ().in_expr->accept_vis (*this);
	    operand.get_split_in_out ().out_expr->accept_vis (*this);
	    break;
	  }
	case RegisterType::Const:
	  {
	    operand.get_const ().anon_const.get_inner_expr ().accept_vis (
	      *this);
	    break;
	  }
	case RegisterType::Sym:
	  {
	    operand.get_sym ().expr->accept_vis (*this);
	    break;
	  }
	case RegisterType::Label:
	  {
	    operand.get_label ().expr->accept_vis (*this);
	    break;
	  }
	}
    }
}

void
DefaultHIRVisitor::walk (LlvmInlineAsm &expr)
{
  for (auto &output : expr.outputs)
    output.expr->accept_vis (*this);
  for (auto &input : expr.inputs)
    input.expr->accept_vis (*this);
}

void
DefaultHIRVisitor::walk (TypeParam &param)
{
  visit_outer_attrs (param);
  for (auto &bounds : param.get_type_param_bounds ())
    bounds->accept_vis (*this);
  if (param.has_type ())
    param.get_type ().accept_vis (*this);
}

void
DefaultHIRVisitor::walk (ConstGenericParam &const_param)
{
  visit_outer_attrs (const_param);
  const_param.get_type ().accept_vis (*this);
  if (const_param.has_default_expression ())
    const_param.get_default_expression ().accept_vis (*this);
}

void
DefaultHIRVisitor::walk (LifetimeWhereClauseItem &item)
{
  item.get_lifetime ().accept_vis (*this);
  for (auto &bound : item.get_lifetime_bounds ())
    bound.accept_vis (*this);
}

void
DefaultHIRVisitor::walk (TypeBoundWhereClauseItem &item)
{
  for (auto &lifetime : item.get_for_lifetimes ())
    lifetime.accept_vis (*this);
  item.get_bound_type ().accept_vis (*this);
  for (auto &param : item.get_type_param_bounds ())
    param->accept_vis (*this);
}

void
DefaultHIRVisitor::walk (Module &module)
{
  visit_outer_attrs (module);
  visit_inner_attrs (module);
  for (auto &item : module.get_items ())
    item->accept_vis (*this);
}

void
DefaultHIRVisitor::walk (ExternCrate &crate)
{
  visit_outer_attrs (crate);
}

void
DefaultHIRVisitor::walk (UseTreeGlob &)
{}

void
DefaultHIRVisitor::walk (UseTreeList &)
{}

void
DefaultHIRVisitor::walk (UseTreeRebind &)
{}

void
DefaultHIRVisitor::walk (UseDeclaration &)
{}

void
DefaultHIRVisitor::visit_function_param (FunctionParam &param)
{
  param.get_param_name ().accept_vis (*this);
  param.get_type ().accept_vis (*this);
}

void
DefaultHIRVisitor::walk (Function &function)
{
  visit_outer_attrs (function);
  for (auto &generic : function.get_generic_params ())
    generic->accept_vis (*this);
  for (auto &param : function.get_function_params ())
    visit_function_param (param);
  if (function.has_return_type ())
    function.get_return_type ().accept_vis (*this);
  if (function.has_where_clause ())
    visit_where_clause (function.get_where_clause ());
  function.get_definition ().accept_vis (*this);
}

void
DefaultHIRVisitor::walk (TypeAlias &type_alias)
{
  visit_outer_attrs (type_alias);
  for (auto &generic : type_alias.get_generic_params ())
    generic->accept_vis (*this);
  if (type_alias.has_where_clause ())
    visit_where_clause (type_alias.get_where_clause ());
  type_alias.get_type_aliased ().accept_vis (*this);
}

void
DefaultHIRVisitor::visit_struct_field (StructField &field)
{
  field.get_field_type ().accept_vis (*this);
}

void
DefaultHIRVisitor::walk (StructStruct &struct_item)
{
  visit_outer_attrs (struct_item);
  for (auto &generic : struct_item.get_generic_params ())
    generic->accept_vis (*this);
  if (struct_item.has_where_clause ())
    visit_where_clause (struct_item.get_where_clause ());
  for (auto &field : struct_item.get_fields ())
    visit_struct_field (field);
}

void
DefaultHIRVisitor::walk (TupleStruct &tuple_struct)
{
  visit_outer_attrs (tuple_struct);
  for (auto &generic : tuple_struct.get_generic_params ())
    generic->accept_vis (*this);
  if (tuple_struct.has_where_clause ())
    visit_where_clause (tuple_struct.get_where_clause ());
  for (auto &field : tuple_struct.get_fields ())
    field.get_field_type ().accept_vis (*this);
}

void
DefaultHIRVisitor::walk (EnumItem &item)
{
  visit_outer_attrs (item);
}

void
DefaultHIRVisitor::walk (EnumItemTuple &item_tuple)
{
  reinterpret_cast<EnumItem &> (item_tuple).accept_vis (*this);
  for (auto &field : item_tuple.get_tuple_fields ())
    field.get_field_type ().accept_vis (*this);
}

void
DefaultHIRVisitor::walk (EnumItemStruct &item_struct)
{
  reinterpret_cast<EnumItem &> (item_struct).accept_vis (*this);
  for (auto &field : item_struct.get_struct_fields ())
    field.get_field_type ().accept_vis (*this);
}

void
DefaultHIRVisitor::walk (EnumItemDiscriminant &item)
{
  reinterpret_cast<EnumItem &> (item).accept_vis (*this);
  item.get_discriminant_expression ().accept_vis (*this);
}

void
DefaultHIRVisitor::walk (Enum &enum_item)
{
  visit_outer_attrs (enum_item);
  for (auto &generic : enum_item.get_generic_params ())
    generic->accept_vis (*this);
  if (enum_item.has_where_clause ())
    visit_where_clause (enum_item.get_where_clause ());
  for (auto &item : enum_item.get_variants ())
    item->accept_vis (*this);
}

void
DefaultHIRVisitor::walk (Union &union_item)
{
  visit_outer_attrs (union_item);
  for (auto &generic : union_item.get_generic_params ())
    generic->accept_vis (*this);
  if (union_item.has_where_clause ())
    visit_where_clause (union_item.get_where_clause ());
  for (auto &variant : union_item.get_variants ())
    visit_struct_field (variant);
}

void
DefaultHIRVisitor::walk (ConstantItem &const_item)
{
  visit_outer_attrs (const_item);
  const_item.get_type ().accept_vis (*this);
  const_item.get_expr ().accept_vis (*this);
}

void
DefaultHIRVisitor::walk (StaticItem &static_item)
{
  visit_outer_attrs (static_item);
  static_item.get_type ().accept_vis (*this);
  static_item.get_expr ().accept_vis (*this);
}

void
DefaultHIRVisitor::visit_self_param (SelfParam &self_param)
{
  if (self_param.has_lifetime ())
    {
      Lifetime lifetime = self_param.get_lifetime ();
      lifetime.accept_vis (*this);
    }
  if (self_param.has_type ())
    self_param.get_type ().accept_vis (*this);
}

void
DefaultHIRVisitor::walk (TraitItemFunc &item)
{
  visit_outer_attrs (item);
  TraitFunctionDecl &decl = item.get_decl ();
  for (auto &generic : decl.get_generic_params ())
    generic->accept_vis (*this);
  if (decl.get_self ().has_value ())
    visit_self_param (decl.get_self ().value ());
  for (auto &param : decl.get_function_params ())
    visit_function_param (param);
  if (decl.has_return_type ())
    decl.get_return_type ().accept_vis (*this);
  if (decl.has_where_clause ())
    visit_where_clause (decl.get_where_clause ());
  if (item.has_definition ())
    item.get_block_expr ().accept_vis (*this);
}

void
DefaultHIRVisitor::walk (TraitItemConst &item)
{
  visit_outer_attrs (item);
  item.get_type ().accept_vis (*this);
  if (item.has_expr ())
    item.get_expr ().accept_vis (*this);
}

void
DefaultHIRVisitor::walk (TraitItemType &item)
{
  visit_outer_attrs (item);
  for (auto &bound : item.get_type_param_bounds ())
    bound->accept_vis (*this);
}

void
DefaultHIRVisitor::visit_where_clause (const WhereClause &where_clause)
{
  for (auto &item : where_clause.get_items ())
    item->accept_vis (*this);
}

void
DefaultHIRVisitor::visit_where_clause (WhereClause &where_clause)
{
  for (auto &item : where_clause.get_items ())
    {
      item->accept_vis (*this);
    }
}

void
DefaultHIRVisitor::walk (WhereClauseItem &node)
{}

void
DefaultHIRVisitor::walk (Trait &trait)
{
  visit_outer_attrs (trait);
  for (auto &generic : trait.get_generic_params ())
    generic->accept_vis (*this);
  if (trait.has_where_clause ())
    visit_where_clause (trait.get_where_clause ());
  for (auto &bound : trait.get_type_param_bounds ())
    bound->accept_vis (*this);
  for (auto &item : trait.get_trait_items ())
    item->accept_vis (*this);
}

void
DefaultHIRVisitor::walk (ImplBlock &impl)
{
  visit_outer_attrs (impl);
  for (auto &generic : impl.get_generic_params ())
    generic->accept_vis (*this);
  impl.get_trait_ref ().accept_vis (*this);
  impl.get_type ().accept_vis (*this);
  if (impl.has_where_clause ())
    visit_where_clause (impl.get_where_clause ());
  visit_inner_attrs (impl);
  for (auto &item : impl.get_impl_items ())
    item->accept_vis (*this);
}

void
DefaultHIRVisitor::walk (ExternalStaticItem &item)
{
  visit_outer_attrs (item);
  item.get_item_type ().accept_vis (*this);
}

void
DefaultHIRVisitor::visit_named_function_param (NamedFunctionParam &param)
{
  param.get_type ().accept_vis (*this);
}

void
DefaultHIRVisitor::walk (ExternalFunctionItem &item)
{
  visit_outer_attrs (item);
  for (auto &generic : item.get_generic_params ())
    generic->accept_vis (*this);
  for (auto &param : item.get_function_params ())
    visit_named_function_param (param);
  if (item.has_return_type ())
    item.get_return_type ().accept_vis (*this);
  if (item.has_where_clause ())
    visit_where_clause (item.get_where_clause ());
}

void
DefaultHIRVisitor::walk (ExternalTypeItem &item)
{
  visit_outer_attrs (item);
}

void
DefaultHIRVisitor::walk (ExternBlock &block)
{
  visit_outer_attrs (block);
  visit_inner_attrs (block);
  for (auto &item : block.get_extern_items ())
    item->accept_vis (*this);
}

void
DefaultHIRVisitor::walk (LiteralPattern &)
{}

void
DefaultHIRVisitor::walk (IdentifierPattern &pattern)
{
  if (pattern.has_subpattern ())
    pattern.get_subpattern ().accept_vis (*this);
}

void
DefaultHIRVisitor::walk (WildcardPattern &)
{}

void
DefaultHIRVisitor::walk (RangePatternBoundLiteral &)
{}

void
DefaultHIRVisitor::walk (RangePatternBoundPath &bound)
{
  bound.get_path ().accept_vis (*this);
}

void
DefaultHIRVisitor::walk (RangePatternBoundQualPath &bound)
{
  bound.get_qualified_path ().accept_vis (*this);
}

void
DefaultHIRVisitor::walk (RangePattern &pattern)
{
  pattern.get_lower_bound ().accept_vis (*this);
  pattern.get_upper_bound ().accept_vis (*this);
}

void
DefaultHIRVisitor::walk (ReferencePattern &pattern)
{
  pattern.get_referenced_pattern ().accept_vis (*this);
}

void
DefaultHIRVisitor::walk (StructPatternFieldTuplePat &field)
{
  visit_outer_attrs (field);
  field.get_tuple_pattern ().accept_vis (*this);
}

void
DefaultHIRVisitor::walk (StructPatternFieldIdentPat &field)
{
  visit_outer_attrs (field);
  field.get_pattern ().accept_vis (*this);
}

void
DefaultHIRVisitor::walk (StructPatternFieldIdent &field)
{
  visit_outer_attrs (field);
}

void
DefaultHIRVisitor::walk (StructPattern &pattern)
{
  pattern.get_path ().accept_vis (*this);
  StructPatternElements &elements = pattern.get_struct_pattern_elems ();
  for (auto &field : elements.get_struct_pattern_fields ())
    field->accept_vis (*this);
}

void
DefaultHIRVisitor::walk (TupleStructItemsNoRange &tuple_items)
{
  for (auto &item : tuple_items.get_patterns ())
    item->accept_vis (*this);
}

void
DefaultHIRVisitor::walk (TupleStructItemsRange &tuple_items)
{
  for (auto &lower : tuple_items.get_lower_patterns ())
    lower->accept_vis (*this);
  for (auto &upper : tuple_items.get_upper_patterns ())
    upper->accept_vis (*this);
}

void
DefaultHIRVisitor::walk (TupleStructPattern &pattern)
{
  pattern.get_path ().accept_vis (*this);
  pattern.get_items ().accept_vis (*this);
}

void
DefaultHIRVisitor::walk (TuplePatternItemsMultiple &tuple_items)
{
  for (auto &pattern : tuple_items.get_patterns ())
    pattern->accept_vis (*this);
}

void
DefaultHIRVisitor::walk (TuplePatternItemsRanged &tuple_items)
{
  for (auto &lower : tuple_items.get_lower_patterns ())
    lower->accept_vis (*this);
  for (auto &upper : tuple_items.get_upper_patterns ())
    upper->accept_vis (*this);
}

void
DefaultHIRVisitor::walk (TuplePattern &pattern)
{
  pattern.get_items ().accept_vis (*this);
}

void
DefaultHIRVisitor::walk (SlicePattern &pattern)
{
  for (auto &item : pattern.get_items ())
    item->accept_vis (*this);
}

void
DefaultHIRVisitor::walk (AltPattern &pattern)
{
  for (auto &item : pattern.get_alts ())
    item->accept_vis (*this);
}

void
DefaultHIRVisitor::walk (EmptyStmt &stmt)
{}

void
DefaultHIRVisitor::walk (LetStmt &stmt)
{
  visit_outer_attrs (stmt);
  stmt.get_pattern ().accept_vis (*this);
  if (stmt.has_type ())
    stmt.get_type ().accept_vis (*this);
  if (stmt.has_init_expr ())
    stmt.get_init_expr ().accept_vis (*this);
}

void
DefaultHIRVisitor::walk (ExprStmt &stmt)
{
  stmt.get_expr ().accept_vis (*this);
}

void
DefaultHIRVisitor::walk (TraitBound &bound)
{
  for (auto &lifetime : bound.get_for_lifetimes ())
    lifetime.accept_vis (*this);
  bound.get_path ().accept_vis (*this);
}

void
DefaultHIRVisitor::walk (ImplTraitType &type)
{
  for (auto &bound : type.get_type_param_bounds ())
    bound->accept_vis (*this);
}

void
DefaultHIRVisitor::walk (TraitObjectType &type)
{
  for (auto &bound : type.get_type_param_bounds ())
    bound->accept_vis (*this);
}

void
DefaultHIRVisitor::walk (ParenthesisedType &type)
{
  type.get_type_in_parens ().accept_vis (*this);
}

void
DefaultHIRVisitor::walk (TupleType &type)
{
  for (auto &elem : type.get_elems ())
    elem->accept_vis (*this);
}

void
DefaultHIRVisitor::walk (NeverType &type)
{}

void
DefaultHIRVisitor::walk (RawPointerType &type)
{
  type.get_type ().accept_vis (*this);
}

void
DefaultHIRVisitor::walk (ReferenceType &type)
{
  if (type.has_lifetime ())
    type.get_lifetime ().accept_vis (*this);
  type.get_base_type ().accept_vis (*this);
}

void
DefaultHIRVisitor::walk (ArrayType &type)
{
  type.get_element_type ().accept_vis (*this);
  type.get_size_expr ().accept_vis (*this);
}

void
DefaultHIRVisitor::walk (SliceType &type)
{
  type.get_element_type ().accept_vis (*this);
}

void
DefaultHIRVisitor::walk (InferredType &type)
{}

void
DefaultHIRVisitor::walk (BareFunctionType &type)
{
  for (auto &lifetime : type.get_for_lifetimes ())
    lifetime.accept_vis (*this);
  for (auto &param : type.get_function_params ())
    param.get_type ().accept_vis (*this);
  if (type.has_return_type ())
    type.get_return_type ().accept_vis (*this);
}

} // namespace HIR
} // namespace Rust