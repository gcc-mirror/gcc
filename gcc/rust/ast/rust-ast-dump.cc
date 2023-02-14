// Copyright (C) 2020-2023 Free Software Foundation, Inc.

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

#include "rust-ast-dump.h"

namespace Rust {
namespace AST {

Indent::Indent () : tabs (0) {}

std::ostream &
operator<< (std::ostream &stream, const Indent &indent)
{
  return stream << std::string (indent.tabs, '\t');
}

void
Indent::increment ()
{
  tabs++;
}

void
Indent::decrement ()
{
  rust_assert (tabs != 0);
  tabs--;
}

Dump::Dump (std::ostream &stream) : stream (stream), indentation (Indent ()) {}

void
Dump::go (AST::Crate &crate)
{
  visit_items_as_lines (crate.items, "");
}

void
Dump::go (AST::Item &item)
{
  item.accept_vis (*this);
}

template <typename T>
void
Dump::visit (std::unique_ptr<T> &node)
{
  node->accept_vis (*this);
}

template <typename T>
void
Dump::visit (T &node)
{
  node.accept_vis (*this);
}

template <typename T>
void
Dump::visit_items_joined_by_separator (T &collection,
				       const std::string &separator,
				       size_t start_offset, size_t end_offset)
{
  if (collection.size () > start_offset)
    {
      visit (collection.at (start_offset));
      auto size = collection.size () - end_offset;
      for (size_t i = start_offset + 1; i < size; i++)
	{
	  stream << separator;
	  visit (collection.at (i));
	}
    }
}

template <typename T>
void
Dump::visit_as_line (T &item, const std::string &trailing)
{
  stream << indentation;
  visit (item);
  stream << trailing << '\n';
}

template <typename T>
void
Dump::visit_items_as_lines (T &collection, const std::string &trailing)
{
  for (auto &item : collection)
    visit_as_line (item, trailing);
}

template <typename T>
void
Dump::visit_items_as_block (T &collection, const std::string &line_trailing,
			    char left_brace, char right_brace)
{
  if (collection.empty ())
    {
      stream << left_brace << right_brace << '\n';
    }
  else
    {
      stream << left_brace << '\n';

      indentation.increment ();
      visit_items_as_lines (collection, line_trailing);
      indentation.decrement ();

      stream << indentation << right_brace << '\n';
    }
}

void
Dump::visit (FunctionParam &param)
{
  visit (param.get_pattern ());
  stream << ": ";
  visit (param.get_type ());
}

void
Dump::visit (Attribute &attrib)
{
  stream << "#[";
  visit_items_joined_by_separator (attrib.get_path ().get_segments (), "::");

  if (attrib.has_attr_input ())
    {
      stream << " = ";

      bool is_literal = attrib.get_attr_input ().get_attr_input_type ()
			== AST::AttrInput::AttrInputType::LITERAL;
      if (is_literal)
	{
	  auto &literal
	    = static_cast<AST::AttrInputLiteral &> (attrib.get_attr_input ());
	  const auto &value = literal.get_literal ().as_string ();

	  stream << "\"" << value << "\"";
	}
      else
	{
	  stream << "FIXME";
	}
    }

  stream << "]";
}

void
Dump::visit (SimplePathSegment &segment)
{
  stream << segment.get_segment_name ();
}

void
Dump::visit (Visibility &vis)
{
  switch (vis.get_vis_type ())
    {
    case Visibility::PUB:
      stream << "pub ";
      break;
    case Visibility::PUB_CRATE:
      stream << "pub(crate) ";
      break;
    case Visibility::PUB_SELF:
      stream << "pub(self) ";
      break;
    case Visibility::PUB_SUPER:
      stream << "pub(super) ";
      break;
    case Visibility::PUB_IN_PATH:
      stream << "pub(in " << vis.get_path ().as_string () << ") ";
      break;
    case Visibility::PRIV:
      break;
    }
}

void
Dump::visit (NamedFunctionParam &param)
{
  stream << param.get_name () << ": ";
  visit (param.get_type ());
}

void
Dump::visit (std::vector<std::unique_ptr<GenericParam>> &params)
{
  stream << "<";
  visit_items_joined_by_separator (params, ", ");
  stream << ">";
}

void
Dump::visit (TupleField &field)
{
  // TODO: do we need to emit outer attrs here?
  visit (field.get_visibility ());
  visit (field.get_field_type ());
}

void
Dump::visit (StructField &field)
{
  // TODO: do we need to emit outer attrs here?
  visit (field.get_visibility ());
  stream << field.get_field_name () << ": ";
  visit (field.get_field_type ());
}

// TODO is this unique by type?
void
Dump::visit (std::vector<LifetimeParam> &for_lifetimes)
{
  // ForLifetimes :
  //     for GenericParams
  //
  // GenericParams :
  //      < >
  //   | < (GenericParam ,)* GenericParam ,? >
  //
  // GenericParam :
  //   OuterAttribute* ( LifetimeParam | TypeParam | ConstParam )
  //
  // LifetimeParam :
  //   LIFETIME_OR_LABEL ( : LifetimeBounds )?

  stream << "for <";
  visit_items_joined_by_separator (for_lifetimes, " + ");
  stream << "> ";
}

void
Dump::visit (FunctionQualifiers &qualifiers)
{
  // Syntax:
  //    `const`? `async`? `unsafe`? (`extern` Abi?)?
  //    unsafe? (extern Abi?)?

  switch (qualifiers.get_const_status ())
    {
    case NONE:
      break;
    case CONST_FN:
      stream << "const ";
      break;
    case ASYNC_FN:
      stream << "async ";
      break;
    }

  if (qualifiers.is_unsafe ())
    stream << "unsafe ";
  if (qualifiers.is_extern ())
    {
      stream << "extern ";
      if (qualifiers.has_abi ())
	stream << "\"" << qualifiers.get_extern_abi () << "\" ";
    }
} // namespace AST

void
Dump::visit (MaybeNamedParam &param)
{
  // Syntax:
  //     OuterAttribute* ( ( IDENTIFIER | _ ) : )? Type

  visit_items_joined_by_separator (param.get_outer_attrs (), " ");
  switch (param.get_param_kind ())
    {
    case MaybeNamedParam::UNNAMED:
      break;
    case MaybeNamedParam::IDENTIFIER:
      stream << " " << param.get_name () << ": ";
      break;
    case MaybeNamedParam::WILDCARD:
      stream << " _: ";
      break;
    }
  visit (param.get_type ());
}

void
Dump::visit (Token &tok)
{
  stream << tok.as_string ();
}

void
Dump::visit (DelimTokenTree &delim_tok_tree)
{
  indentation.increment ();
  stream << '\n' << indentation;

  auto tokens = delim_tok_tree.to_token_stream ();
  visit_items_joined_by_separator (tokens, " ");

  indentation.decrement ();
  stream << '\n' << indentation;
}

void
Dump::visit (AttrInputMetaItemContainer &)
{}

void
Dump::visit (IdentifierExpr &ident_expr)
{
  stream << ident_expr.get_ident ();
}

void
Dump::visit (Lifetime &lifetime)
{
  // Syntax:
  // Lifetime :
  // 	LIFETIME_OR_LABEL
  // 	| 'static
  // 	| '_
  stream << lifetime.as_string ();
}

void
Dump::visit (LifetimeParam &lifetime_param)
{
  // Syntax:
  //   LIFETIME_OR_LABEL ( : LifetimeBounds )?
  // LifetimeBounds :
  //   ( Lifetime + )* Lifetime?

  // TODO what to do with outer attr? They are not mentioned in the reference.

  auto lifetime = lifetime_param.get_lifetime ();
  visit (lifetime);

  if (lifetime_param.has_lifetime_bounds ())
    {
      stream << ": ";
      visit_items_joined_by_separator (lifetime_param.get_lifetime_bounds (),
				       " + ");
    }
}

void
Dump::visit (ConstGenericParam &)
{}

// rust-path.h
void
Dump::visit (PathInExpression &path)
{
  stream << path.as_string ();
}

void
Dump::visit (TypePathSegment &segment)
{
  // Syntax:
  //    PathIdentSegment

  stream << segment.get_ident_segment ().as_string ();
}

void
Dump::visit (TypePathSegmentGeneric &segment)
{
  // Syntax:
  //    PathIdentSegment `::`? (GenericArgs)?
  // GenericArgs :
  //    `<` `>`
  //    | `<` ( GenericArg `,` )* GenericArg `,`? `>`

  stream << segment.get_ident_segment ().as_string ();

  if (segment.get_separating_scope_resolution ())
    stream << "::";

  stream << "<";

  {
    // Here we join 3 lists (each possibly empty) with a separator.

    auto &lifetime_args = segment.get_generic_args ().get_lifetime_args ();
    auto &generic_args = segment.get_generic_args ().get_generic_args ();
    auto &binding_args = segment.get_generic_args ().get_binding_args ();

    visit_items_joined_by_separator (lifetime_args, ", ");
    if (!lifetime_args.empty ()
	&& (!generic_args.empty () || !binding_args.empty ()))
      {
	// Insert separator if some items have been already emitted and some
	// more are to be emitted from any of the following collections.
	stream << ", ";
      }
    visit_items_joined_by_separator (generic_args, ", ");
    if (!generic_args.empty () && !binding_args.empty ())
      {
	// Insert separator if some item vas emitted from the previous
	// collection and more are to be emitted from the last.
	stream << ", ";
      }
    visit_items_joined_by_separator (binding_args, ", ");
  }

  stream << ">";
}

void
Dump::visit (GenericArgsBinding &binding)
{
  // Syntax:
  //    IDENTIFIER `=` Type

  stream << binding.get_identifier () << " << ";
  visit (binding.get_type ());
}

void
Dump::visit (GenericArg &arg)
{
  // `GenericArg` implements `accept_vis` but it is not useful for this case as
  // it ignores unresolved cases (`Kind::Either`).

  switch (arg.get_kind ())
    {
    case GenericArg::Kind::Const:
      visit (arg.get_expression ());
      break;
    case GenericArg::Kind::Type:
      visit (arg.get_type ());
      break;
    case GenericArg::Kind::Either:
      stream << arg.get_path ();
      break;
    case GenericArg::Kind::Error:
      gcc_unreachable ();
    }
} // namespace AST

void
Dump::visit (TypePathSegmentFunction &segment)
{
  // Syntax:
  //   PathIdentSegment `::`? (TypePathFn)?

  stream << segment.get_ident_segment ().as_string ();

  if (segment.get_separating_scope_resolution ())
    stream << "::";

  if (!segment.is_ident_only ())
    visit (segment.get_type_path_function ());
}

void
Dump::visit (TypePathFunction &type_path_fn)
{
  // Syntax:
  //   `(` TypePathFnInputs? `)` (`->` Type)?
  // TypePathFnInputs :
  //   Type (`,` Type)* `,`?

  stream << '(';
  if (type_path_fn.has_inputs ())
    visit_items_joined_by_separator (type_path_fn.get_params (), ", ");
  stream << ')';

  if (type_path_fn.has_return_type ())
    {
      stream << "->";
      visit (type_path_fn.get_return_type ());
    }
}

void
Dump::visit (TypePath &path)
{
  // Syntax:
  //    `::`? TypePathSegment (`::` TypePathSegment)*

  if (path.has_opening_scope_resolution_op ())
    stream << "::";
  visit_items_joined_by_separator (path.get_segments (), "::");
}

void
Dump::visit (QualifiedPathInExpression &path)
{
  stream << path.as_string ();
}

void
Dump::visit (QualifiedPathInType &)
{}

// rust-expr.h
void
Dump::visit (LiteralExpr &expr)
{
  stream << expr.as_string ();
}

void
Dump::visit (AttrInputLiteral &)
{}

void
Dump::visit (MetaItemLitExpr &)
{}

void
Dump::visit (MetaItemPathLit &)
{}

void
Dump::visit (BorrowExpr &expr)
{
  stream << '&';
  if (expr.get_is_double_borrow ())
    stream << '&';
  if (expr.get_is_mut ())
    stream << "mut ";

  visit (expr.get_borrowed_expr ());
}

void
Dump::visit (DereferenceExpr &expr)
{
  stream << '*';
  visit (expr.get_dereferenced_expr ());
}

void
Dump::visit (ErrorPropagationExpr &expr)
{
  visit (expr.get_propagating_expr ());
  stream << '?';
}

void
Dump::visit (NegationExpr &expr)
{
  switch (expr.get_expr_type ())
    {
    case NegationOperator::NEGATE:
      stream << '-';
      break;
    case NegationOperator::NOT:
      stream << '!';
      break;
    }
  visit (expr.get_negated_expr ());
}

void
Dump::visit (ArithmeticOrLogicalExpr &expr)
{
  auto op = "";
  switch (expr.get_expr_type ())
    {
    case ArithmeticOrLogicalOperator::ADD:
      op = "+";
      break;

    case ArithmeticOrLogicalOperator::SUBTRACT:
      op = "-";
      break;

    case ArithmeticOrLogicalOperator::MULTIPLY:
      op = "*";
      break;

    case ArithmeticOrLogicalOperator::DIVIDE:
      op = "/";
      break;

    case ArithmeticOrLogicalOperator::MODULUS:
      op = "%";
      break;

    case ArithmeticOrLogicalOperator::BITWISE_AND:
      op = "&";
      break;

    case ArithmeticOrLogicalOperator::BITWISE_OR:
      op = "|";
      break;

    case ArithmeticOrLogicalOperator::BITWISE_XOR:
      op = "^";
      break;

    case ArithmeticOrLogicalOperator::LEFT_SHIFT:
      op = "<<";
      break;

    case ArithmeticOrLogicalOperator::RIGHT_SHIFT:
      op = ">>";
      break;
    }

  visit (expr.get_left_expr ());
  stream << " " << op << " ";
  visit (expr.get_right_expr ());
}

void
Dump::visit (ComparisonExpr &expr)
{
  auto op = "";
  switch (expr.get_expr_type ())
    {
    case ComparisonOperator::EQUAL:
      op = "==";
      break;
    case ComparisonOperator::NOT_EQUAL:
      op = "!=";
      break;

    case ComparisonOperator::GREATER_THAN:
      op = ">";
      break;

    case ComparisonOperator::LESS_THAN:
      op = "<";
      break;

    case ComparisonOperator::GREATER_OR_EQUAL:
      op = ">=";
      break;

    case ComparisonOperator::LESS_OR_EQUAL:
      op = "<=";
      break;
    }

  visit (expr.get_left_expr ());
  stream << " " << op << " ";
  visit (expr.get_right_expr ());
}

void
Dump::visit (LazyBooleanExpr &expr)
{
  auto op = "";
  switch (expr.get_expr_type ())
    {
    case LazyBooleanOperator::LOGICAL_AND:
      op = "&&";
      break;
    case LazyBooleanOperator::LOGICAL_OR:
      op = "||";
      break;
    }

  visit (expr.get_left_expr ());
  stream << " " << op << " ";
  visit (expr.get_right_expr ());
}

void
Dump::visit (TypeCastExpr &expr)
{
  visit (expr.get_casted_expr ());
  stream << " as ";
  visit (expr.get_type_to_cast_to ());
}

void
Dump::visit (AssignmentExpr &expr)
{
  expr.visit_lhs (*this);
  stream << " = ";
  expr.visit_rhs (*this);
}

void
Dump::visit (CompoundAssignmentExpr &expr)
{
  auto op = "";
  switch (expr.get_expr_type ())
    {
    case CompoundAssignmentOperator::ADD:
      op = "+";
      break;

    case CompoundAssignmentOperator::SUBTRACT:
      op = "-";
      break;

    case CompoundAssignmentOperator::MULTIPLY:
      op = "*";
      break;

    case CompoundAssignmentOperator::DIVIDE:
      op = "/";
      break;

    case CompoundAssignmentOperator::MODULUS:
      op = "%";
      break;

    case CompoundAssignmentOperator::BITWISE_AND:
      op = "&";
      break;

    case CompoundAssignmentOperator::BITWISE_OR:
      op = "|";
      break;

    case CompoundAssignmentOperator::BITWISE_XOR:
      op = "^";
      break;

    case CompoundAssignmentOperator::LEFT_SHIFT:
      op = "<<";
      break;

    case CompoundAssignmentOperator::RIGHT_SHIFT:
      op = ">>";
      break;
    }

  visit (expr.get_left_expr ());
  stream << " " << op << "= ";
  visit (expr.get_right_expr ());
}

void
Dump::visit (GroupedExpr &expr)
{
  stream << '(';
  visit (expr.get_expr_in_parens ());
  stream << ')';
}

void
Dump::visit (ArrayElemsValues &elems)
{
  visit_items_joined_by_separator (elems.get_values (), ", ");
}

void
Dump::visit (ArrayElemsCopied &elems)
{
  visit (elems.get_elem_to_copy ());
  stream << "; ";
  visit (elems.get_num_copies ());
}

void
Dump::visit (ArrayExpr &expr)
{
  stream << '[';
  visit (expr.get_array_elems ());
  stream << ']';
}

void
Dump::visit (ArrayIndexExpr &expr)
{
  visit (expr.get_array_expr ());
  stream << '[';
  visit (expr.get_index_expr ());
  stream << ']';
}

void
Dump::visit (TupleExpr &)
{}

void
Dump::visit (TupleIndexExpr &)
{}

void
Dump::visit (StructExprStruct &)
{}

void
Dump::visit (StructExprFieldIdentifier &)
{}

void
Dump::visit (StructExprFieldIdentifierValue &)
{}

void
Dump::visit (StructExprFieldIndexValue &)
{}

void
Dump::visit (StructExprStructFields &)
{}

void
Dump::visit (StructExprStructBase &)
{}

void
Dump::visit (CallExpr &expr)
{
  visit (expr.get_function_expr ());

  stream << '(' << '\n';
  indentation.increment ();

  visit_items_as_lines (expr.get_params (), ",");

  indentation.decrement ();
  stream << indentation << ')';
}

void
Dump::visit (MethodCallExpr &)
{}

void
Dump::visit (FieldAccessExpr &)
{}

void
Dump::visit (ClosureExprInner &)
{}

void
Dump::visit (BlockExpr &expr)
{
  stream << "{\n";
  indentation.increment ();

  visit_items_as_lines (expr.get_statements (), ";");

  if (expr.has_tail_expr ())
    visit_as_line (expr.get_tail_expr (), " /* tail expr */\n");

  indentation.decrement ();
  stream << indentation << "}\n";
}

void
Dump::visit (ClosureExprInnerTyped &)
{}

void
Dump::visit (ContinueExpr &)
{}

void
Dump::visit (BreakExpr &)
{}

void
Dump::visit (RangeFromToExpr &expr)
{
  visit (expr.get_from_expr ());
  stream << "..";
  visit (expr.get_to_expr ());
}

void
Dump::visit (RangeFromExpr &expr)
{
  visit (expr.get_from_expr ());
  stream << "..";
}

void
Dump::visit (RangeToExpr &expr)
{
  stream << "..";
  visit (expr.get_to_expr ());
}

void
Dump::visit (RangeFullExpr &)
{
  stream << "..";
}

void
Dump::visit (RangeFromToInclExpr &expr)
{
  visit (expr.get_from_expr ());
  stream << "..=";
  visit (expr.get_to_expr ());
}

void
Dump::visit (RangeToInclExpr &expr)
{
  stream << "..=";
  visit (expr.get_to_expr ());
}

void
Dump::visit (ReturnExpr &)
{}

void
Dump::visit (UnsafeBlockExpr &)
{}

void
Dump::visit (LoopExpr &)
{}

void
Dump::visit (WhileLoopExpr &)
{}

void
Dump::visit (WhileLetLoopExpr &)
{}

void
Dump::visit (ForLoopExpr &)
{}

void
Dump::visit (IfExpr &expr)
{
  stream << "if ";
  visit (expr.get_condition_expr ());
  stream << " ";
  visit (expr.get_if_block ());
}

void
Dump::visit (IfExprConseqElse &expr)
{
  stream << "if ";
  visit (expr.get_condition_expr ());
  stream << " ";
  visit (expr.get_if_block ());
  stream << indentation << "else ";
  visit (expr.get_else_block ());
}

void
Dump::visit (IfExprConseqIf &expr)
{
  stream << "if ";
  visit (expr.get_condition_expr ());
  stream << " ";
  visit (expr.get_if_block ());
  stream << indentation << "else ";
  // The "if" part of the "else if" is printed by the next visitor
  visit (expr.get_conseq_if_expr ());
}

void
Dump::visit (IfExprConseqIfLet &)
{}

void
Dump::visit (IfLetExpr &)
{}

void
Dump::visit (IfLetExprConseqElse &)
{}

void
Dump::visit (IfLetExprConseqIf &)
{}

void
Dump::visit (IfLetExprConseqIfLet &)
{}

void
Dump::visit (MatchExpr &)
{}

void
Dump::visit (AwaitExpr &)
{}

void
Dump::visit (AsyncBlockExpr &)
{}

// rust-item.h
void
Dump::visit (TypeParam &param)
{
  // Syntax:
  //    IDENTIFIER( : TypeParamBounds? )? ( = Type )?
  // TypeParamBounds :
  //    TypeParamBound ( + TypeParamBound )* +?

  stream << param.get_type_representation ();
  if (param.has_type_param_bounds ())
    {
      stream << ": ";
      visit_items_joined_by_separator (param.get_type_param_bounds (), " + ");
    }
  if (param.has_type ())
    {
      stream << " = ";
      visit (param.get_type ());
    }
}

void
Dump::visit (WhereClause &rule)
{
  // Syntax:
  // 	where ( WhereClauseItem , )* WhereClauseItem ?
  // WhereClauseItem :
  // 	LifetimeWhereClauseItem
  //  	| TypeBoundWhereClauseItem

  stream << " where\n";
  indentation.increment ();
  visit_items_as_lines (rule.get_items (), ",");
  indentation.decrement ();
}

void
Dump::visit (LifetimeWhereClauseItem &item)
{
  // Syntax:
  // 	Lifetime : LifetimeBounds
  // LifetimeBounds :
  //   ( Lifetime + )* Lifetime?

  visit (item.get_lifetime ());
  stream << ": ";
  visit_items_joined_by_separator (item.get_lifetime_bounds (), " + ");
}

void
Dump::visit (TypeBoundWhereClauseItem &item)
{
  // Syntax:
  // 	ForLifetimes? Type : TypeParamBounds?
  // TypeParamBounds :
  // 	TypeParamBound ( + TypeParamBound )* +?
  // TypeParamBound :
  //    Lifetime | TraitBound

  if (item.has_for_lifetimes ())
    visit (item.get_for_lifetimes ());

  visit (item.get_type ());
  stream << ": ";

  visit_items_joined_by_separator (item.get_type_param_bounds (), " + ");
}

void
Dump::visit (Method &method)
{
  visit (method.get_visibility ());
  stream << "fn " << method.get_method_name () << '(';

  stream << method.get_self_param ().as_string ();
  if (!method.get_function_params ().empty ())
    {
      stream << ", ";
      visit_items_joined_by_separator (method.get_function_params (), ", ");
    }

  stream << ") ";

  if (method.has_return_type ())
    {
      stream << "-> ";
      visit (method.get_return_type ());
      stream << " ";
    }

  auto &block = method.get_definition ();
  if (!block)
    stream << ';';
  else
    visit (block);

  stream << '\n';
}

void
Dump::visit (Module &module)
{
  //  Syntax:
  //	mod IDENTIFIER ;
  //     | mod IDENTIFIER {
  //	  InnerAttribute*
  //	  Item*
  //	}

  visit (module.get_visibility ());
  stream << "mod " << module.get_name ();

  if (module.get_kind () == Module::UNLOADED)
    {
      stream << ";\n";
    }
  else /* Module::LOADED */
    {
      stream << " {\n";

      indentation.increment ();

      visit_items_as_lines (module.get_inner_attrs ());
      visit_items_as_lines (module.get_items ());

      indentation.decrement ();

      stream << indentation << "}\n";
    }
}

void
Dump::visit (ExternCrate &)
{}

void
Dump::visit (UseTreeGlob &)
{}

void
Dump::visit (UseTreeList &)
{}

void
Dump::visit (UseTreeRebind &)
{}

void
Dump::visit (UseDeclaration &)
{}

void
Dump::visit (Function &function)
{
  // Syntax:
  //   FunctionQualifiers fn IDENTIFIER GenericParams?
  //      ( FunctionParameters? )
  //      FunctionReturnType? WhereClause?
  //      ( BlockExpression | ; )

  visit (function.get_visibility ());

  stream << "fn " << function.get_function_name ();
  if (function.has_generics ())
    visit (function.get_generic_params ());

  stream << '(';
  visit_items_joined_by_separator (function.get_function_params ());
  stream << ") ";

  if (function.has_return_type ())
    {
      stream << "-> ";
      visit (function.get_return_type ());
      stream << " ";
    }

  if (function.has_where_clause ())
    visit (function.get_where_clause ());

  auto &block = function.get_definition ();
  if (!block)
    stream << ';';
  else
    visit (block);

  stream << '\n';
}

void
Dump::visit (TypeAlias &type_alias)
{
  // Syntax:
  // Visibility? type IDENTIFIER GenericParams? WhereClause? = Type;

  // Note: Associated types are handled by `AST::TraitItemType`.

  if (type_alias.has_visibility ())
    visit (type_alias.get_visibility ());
  stream << "type " << type_alias.get_new_type_name ();
  if (type_alias.has_generics ())
    visit (type_alias.get_generic_params ());
  if (type_alias.has_where_clause ())
    visit (type_alias.get_where_clause ());
  stream << " = ";
  visit (type_alias.get_type_aliased ());
}

void
Dump::visit (StructStruct &struct_item)
{
  stream << "struct " << struct_item.get_identifier ();
  if (struct_item.has_generics ())
    visit (struct_item.get_generic_params ());
  if (struct_item.has_where_clause ())
    visit (struct_item.get_where_clause ());
  if (struct_item.is_unit_struct ())
    stream << ";\n";
  else
    visit_items_as_block (struct_item.get_fields (), ",");
}

void
Dump::visit (TupleStruct &tuple_struct)
{
  stream << "struct " << tuple_struct.get_identifier ();
  if (tuple_struct.has_generics ())
    visit (tuple_struct.get_generic_params ());
  if (tuple_struct.has_where_clause ())
    visit (tuple_struct.get_where_clause ());

  stream << '(';
  visit_items_joined_by_separator (tuple_struct.get_fields (), ", ");
  stream << ");\n";
}

void
Dump::visit (EnumItem &item)
{
  stream << item.get_identifier ();
}

void
Dump::visit (EnumItemTuple &item)
{
  stream << item.get_identifier () << '(';
  visit_items_joined_by_separator (item.get_tuple_fields (), ", ");
  stream << ')';
}

void
Dump::visit (EnumItemStruct &item)
{
  stream << item.get_identifier ();
  visit_items_as_block (item.get_struct_fields (), ",");
}

void
Dump::visit (EnumItemDiscriminant &item)
{
  stream << item.get_identifier () << " = ";
  visit (item.get_expr ());
}

void
Dump::visit (Enum &enum_item)
{
  stream << "enum " << enum_item.get_identifier ();
  if (enum_item.has_generics ())
    visit (enum_item.get_generic_params ());
  if (enum_item.has_where_clause ())
    visit (enum_item.get_where_clause ());

  visit_items_as_block (enum_item.get_variants (), ",");
}

void
Dump::visit (Union &union_item)
{
  stream << "union " << union_item.get_identifier ();
  if (union_item.has_generics ())
    visit (union_item.get_generic_params ());
  if (union_item.has_where_clause ())
    visit (union_item.get_where_clause ());

  visit_items_as_block (union_item.get_variants (), ",");
}

void
Dump::visit (ConstantItem &)
{}

void
Dump::visit (StaticItem &)
{}

void
Dump::visit_function_common (std::unique_ptr<Type> &return_type,
			     std::unique_ptr<BlockExpr> &block)
{
  // FIXME: This should format the `<vis> fn <name> ( [args] )` as well
  if (return_type)
    {
      stream << "-> ";
      visit (return_type);
    }

  if (block)
    {
      if (return_type)
	{
	  stream << ' ';
	  visit (block);
	}
    }
  else
    stream << ";\n";
}

void
Dump::visit (TraitItemFunc &item)
{
  auto func = item.get_trait_function_decl ();
  stream << "fn " << func.get_identifier () << '(';

  visit_items_joined_by_separator (func.get_function_params ());

  stream << ") ";

  visit_function_common (func.get_return_type (), item.get_definition ());
}

void
Dump::visit (TraitItemMethod &item)
{
  auto method = item.get_trait_method_decl ();

  // FIXME: Can we have visibility here?
  // emit_visibility (method.get_visibility ());
  stream << "fn " << method.get_identifier () << '(';

  stream << method.get_self_param ().as_string ();

  if (!method.get_function_params ().empty ())
    {
      stream << ", ";
      visit_items_joined_by_separator (method.get_function_params (), ", ");
    }

  stream << ") ";

  visit_function_common (method.get_return_type (), item.get_definition ());
}

void
Dump::visit (TraitItemConst &item)
{
  stream << indentation << "const " << item.get_identifier () << ": ";
  visit (item.get_type ());
  stream << ";\n";
}

void
Dump::visit (TraitItemType &item)
{
  stream << indentation << "type " << item.get_identifier () << ";\n";
}

void
Dump::visit (Trait &trait)
{
  for (auto &attr : trait.get_outer_attrs ())
    {
      visit (attr);
      stream << "\n" << indentation;
    }

  visit (trait.get_visibility ());

  stream << "trait " << trait.get_identifier ();

  // Traits actually have an implicit Self thrown at the start, so we must
  // expect the number of generic params to be > 1
  if (trait.get_generic_params ().size () > 1)
    {
      stream << "<";
      visit_items_joined_by_separator (trait.get_generic_params (), ", ", 1);
      stream << ">";
    }

  visit_items_as_block (trait.get_trait_items (), "");
}

void
Dump::visit (InherentImpl &impl)
{
  stream << "impl ";

  // FIXME: Handle generics

  visit (impl.get_type ());

  if (impl.has_where_clause ())
    visit (impl.get_where_clause ());

  // FIXME: Handle inner attributes

  visit_items_as_block (impl.get_impl_items (), "");
}

void
Dump::visit (TraitImpl &impl)
{
  stream << "impl ";
  visit (impl.get_trait_path ());
  stream << " for ";
  visit (impl.get_type ());
  stream << " {\n";

  indentation.increment ();

  for (auto &item : impl.get_impl_items ())
    {
      stream << indentation;
      visit (item);
    }

  indentation.decrement ();
  stream << "\n}\n";
}

void
Dump::visit (ExternalStaticItem &)
{}

void
Dump::visit (ExternalFunctionItem &function)
{
  visit (function.get_visibility ());

  stream << "fn " << function.get_identifier () << '(';

  visit_items_joined_by_separator (function.get_function_params ());

  stream << ')';
  if (function.has_return_type ())
    {
      stream << "-> ";
      visit (function.get_return_type ());
    }
}

void
Dump::visit (ExternBlock &block)
{
  stream << "extern ";

  if (block.has_abi ())
    stream << "\"" << block.get_abi () << "\" ";

  visit_items_as_block (block.get_extern_items (), ";");
}

static std::pair<char, char>
get_delimiters (DelimType delim)
{
  auto start_delim = '\0';
  auto end_delim = '\0';

  switch (delim)
    {
    case PARENS:
      start_delim = '(';
      end_delim = ')';
      break;
    case SQUARE:
      start_delim = '[';
      end_delim = ']';
      break;
    case CURLY:
      start_delim = '{';
      end_delim = '}';
      break;
    }

  return {start_delim, end_delim};
}

void
Dump::visit (MacroMatchFragment &match)
{
  stream << '$' << match.get_ident () << ':'
	 << match.get_frag_spec ().as_string ();
}

void
Dump::visit (MacroMatchRepetition &repetition)
{
  stream << "$(";

  visit_items_joined_by_separator (repetition.get_matches (), " ");

  auto op_char = '\0';
  switch (repetition.get_op ())
    {
    case MacroMatchRepetition::ANY:
      op_char = '*';
      break;
    case MacroMatchRepetition::ONE_OR_MORE:
      op_char = '+';
      break;
    case MacroMatchRepetition::ZERO_OR_ONE:
      op_char = '?';
      break;
    case MacroMatchRepetition::NONE:
      break;
    }

  stream << ')';

  if (repetition.has_sep ())
    stream << repetition.get_sep ()->as_string ();

  stream << op_char;
}

void
Dump::visit (MacroMatcher &matcher)
{
  auto delimiters = get_delimiters (matcher.get_delim_type ());

  stream << delimiters.first;

  visit_items_joined_by_separator (matcher.get_matches (), " ");

  stream << delimiters.second;
}

void
Dump::visit (MacroRule &rule)
{
  visit (rule.get_matcher ());
  stream << " => ";
  visit (rule.get_transcriber ().get_token_tree ());
  stream << ";";
}

void
Dump::visit (MacroRulesDefinition &rules_def)
{
  for (auto &outer_attr : rules_def.get_outer_attrs ())
    visit (outer_attr);

  stream << "macro_rules! " << rules_def.get_rule_name ();

  visit_items_as_block (rules_def.get_rules (), ";");
}

void
Dump::visit (MacroInvocation &)
{}

void
Dump::visit (MetaItemPath &)
{}

void
Dump::visit (MetaItemSeq &)
{}

void
Dump::visit (MetaWord &)
{}

void
Dump::visit (MetaNameValueStr &)
{}

void
Dump::visit (MetaListPaths &)
{}

void
Dump::visit (MetaListNameValueStr &)
{}

// rust-pattern.h
void
Dump::visit (LiteralPattern &)
{}

void
Dump::visit (IdentifierPattern &pattern)
{
  stream << pattern.get_ident ();
}

void
Dump::visit (WildcardPattern &)
{}

// void Dump::visit(RangePatternBound& ){}

void
Dump::visit (RangePatternBoundLiteral &)
{}

void
Dump::visit (RangePatternBoundPath &)
{}

void
Dump::visit (RangePatternBoundQualPath &)
{}

void
Dump::visit (RangePattern &)
{}

void
Dump::visit (ReferencePattern &)
{}

// void Dump::visit(StructPatternField& ){}

void
Dump::visit (StructPatternFieldTuplePat &)
{}

void
Dump::visit (StructPatternFieldIdentPat &)
{}

void
Dump::visit (StructPatternFieldIdent &)
{}

void
Dump::visit (StructPattern &)
{}

// void Dump::visit(TupleStructItems& ){}

void
Dump::visit (TupleStructItemsNoRange &)
{}

void
Dump::visit (TupleStructItemsRange &)
{}

void
Dump::visit (TupleStructPattern &)
{}

// void Dump::visit(TuplePatternItems& ){}

void
Dump::visit (TuplePatternItemsMultiple &)
{}

void
Dump::visit (TuplePatternItemsRanged &)
{}

void
Dump::visit (TuplePattern &)
{}

void
Dump::visit (GroupedPattern &)
{}

void
Dump::visit (SlicePattern &)
{}

void
Dump::visit (AltPattern &)
{}

// rust-stmt.h
void
Dump::visit (EmptyStmt &)
{}

void
Dump::visit (LetStmt &stmt)
{
  stream << "let ";
  auto &pattern = stmt.get_pattern ();
  if (pattern)
    visit (pattern);

  if (stmt.has_type ())
    {
      stream << ": ";
      visit (stmt.get_type ());
    }

  if (stmt.has_init_expr ())
    {
      stream << " = ";
      visit (stmt.get_init_expr ());
    }
}

void
Dump::visit (ExprStmtWithoutBlock &stmt)
{
  visit (stmt.get_expr ());
}

void
Dump::visit (ExprStmtWithBlock &stmt)
{
  visit (stmt.get_expr ());
}

// rust-type.h
void
Dump::visit (TraitBound &bound)
{
  // Syntax:
  //      ?? ForLifetimes? TypePath
  //   | ( ?? ForLifetimes? TypePath )

  if (bound.has_opening_question_mark ())
    stream << "? ";

  if (bound.has_for_lifetimes ())
    visit (bound.get_for_lifetimes ());

  visit (bound.get_type_path ());
}

void
Dump::visit (ImplTraitType &type)
{
  // Syntax:
  //    impl TypeParamBounds
  // TypeParamBounds :
  //    TypeParamBound ( + TypeParamBound )* +?

  stream << "impl ";
  visit_items_joined_by_separator (type.get_type_param_bounds (), " + ");
}

void
Dump::visit (TraitObjectType &type)
{
  // Syntax:
  //   dyn? TypeParamBounds
  // TypeParamBounds :
  //   TypeParamBound ( + TypeParamBound )* +?

  if (type.is_dyn ())
    stream << "dyn ";
  visit_items_joined_by_separator (type.get_type_param_bounds (), " + ");
}

void
Dump::visit (ParenthesisedType &type)
{
  // Syntax:
  //    ( Type )

  stream << "(";
  visit (type.get_type_in_parens ());
  stream << ")";
}

void
Dump::visit (ImplTraitTypeOneBound &type)
{
  // Syntax:
  //    impl TraitBound

  stream << "impl ";
  visit (type.get_trait_bound ());
}

void
Dump::visit (TraitObjectTypeOneBound &type)
{
  // Syntax:
  //    dyn? TraitBound

  if (type.is_dyn ())
    stream << "dyn ";
  visit (type.get_trait_bound ());
}

void
Dump::visit (TupleType &type)
{
  // Syntax:
  //   ( )
  //   | ( ( Type , )+ Type? )

  stream << '(';
  visit_items_joined_by_separator (type.get_elems (), ", ");
  stream << ')';
}

void
Dump::visit (NeverType &)
{
  // Syntax:
  //  !

  stream << '!';
}

void
Dump::visit (RawPointerType &type)
{
  // Syntax:
  //    * ( mut | const ) TypeNoBounds

  if (type.get_pointer_type () == RawPointerType::MUT)
    stream << "*mut ";
  else /* RawPointerType::CONST */
    stream << "*const ";

  visit (type.get_type_pointed_to ());
}

void
Dump::visit (ReferenceType &type)
{
  // Syntax:
  //    & Lifetime? mut? TypeNoBounds

  stream << '&';

  if (type.has_lifetime ())
    {
      visit (type.get_lifetime ());
      stream << ' ';
    }

  if (type.get_has_mut ())
    stream << "mut ";

  visit (type.get_type_referenced ());
}

void
Dump::visit (ArrayType &type)
{
  // Syntax:
  //    [ Type ; Expression ]

  stream << '[';
  visit (type.get_elem_type ());
  stream << "; ";
  visit (type.get_size_expr ());
  stream << ']';
}

void
Dump::visit (SliceType &type)
{
  // Syntax:
  //    [ Type ]

  stream << '[';
  visit (type.get_elem_type ());
  stream << ']';
}

void
Dump::visit (InferredType &)
{
  // Syntax:
  //    _

  stream << "_";
}

void
Dump::visit (BareFunctionType &type)
{
  // Syntax:
  //    ForLifetimes? FunctionTypeQualifiers fn
  //      ( FunctionParametersMaybeNamedVariadic? ) BareFunctionReturnType?
  //
  //    BareFunctionReturnType:
  //      -> TypeNoBounds
  //
  //    FunctionParametersMaybeNamedVariadic :
  //      MaybeNamedFunctionParameters | MaybeNamedFunctionParametersVariadic
  //
  //    MaybeNamedFunctionParameters :
  //      MaybeNamedParam ( , MaybeNamedParam )* ,?
  //
  //    MaybeNamedFunctionParametersVariadic :
  //      ( MaybeNamedParam , )* MaybeNamedParam , OuterAttribute* ...

  if (type.has_for_lifetimes ())
    visit (type.get_for_lifetimes ());

  visit (type.get_function_qualifiers ());

  stream << "fn (";

  visit_items_joined_by_separator (type.get_function_params (), ", ");

  if (type.is_variadic ())
    {
      stream << ", ";
      visit_items_joined_by_separator (type.get_variadic_attr (), " ");
      stream << "...";
    }

  stream << ')';

  if (type.has_return_type ())
    {
      stream << " -> ";
      visit (type.get_return_type ());
    }
}

} // namespace AST
} // namespace Rust
