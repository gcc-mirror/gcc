/* This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>. */

#ifndef RUST_PARSE_H
#define RUST_PARSE_H

#include "rust-item.h"
#include "rust-lex.h"
#include "rust-ast-full.h"
#include "rust-diagnostics.h"

#include "expected.h"

namespace Rust {
/* HACK: used to resolve the expression-or-statement problem at the end of a
 * block by allowing either to be returned (technically). Tagged union would
 * probably take up the same amount of space. */
struct ExprOrStmt
{
  std::unique_ptr<AST::Expr> expr;
  std::unique_ptr<AST::Stmt> stmt;

  /* I was going to resist the urge to make this a real class and make it POD,
   * but construction in steps is too difficult. So it'll just also have a
   * constructor. */

  // expression constructor
  ExprOrStmt (std::unique_ptr<AST::Expr> expr) : expr (std::move (expr)) {}

  // statement constructor
  ExprOrStmt (std::unique_ptr<AST::Stmt> stmt) : stmt (std::move (stmt)) {}

  // macro constructor
  ExprOrStmt (std::unique_ptr<AST::MacroInvocation> macro)
    : expr (std::move (macro))
  {}

  // Returns whether this object is in an error state.
  bool is_error () const
  {
    return (expr == nullptr && stmt == nullptr)
	   || (expr != nullptr && stmt != nullptr);
  }

  // Returns an error state object.
  static ExprOrStmt create_error () { return ExprOrStmt (nullptr, nullptr); }

  ~ExprOrStmt () = default;

  /* no copy constructors/assignment as simple object like this shouldn't
   * require it */

  // move constructors
  ExprOrStmt (ExprOrStmt &&other) = default;
  ExprOrStmt &operator= (ExprOrStmt &&other) = default;

private:
  // private constructor only used for creating error state expr or stmt objects
  ExprOrStmt (AST::Expr *expr, AST::Stmt *stmt) : expr (expr), stmt (stmt) {}

  // make this work: have a disambiguation specifically for known statements
  // (i.e. ';' and 'let'). then, have a special "parse expr or stmt" function
  // that returns this type. inside it, it parses an expression, and then
  // determines whether to return expr or stmt via whether the next token is a
  // semicolon. should be able to disambiguate inside that function between
  // stmts with blocks and without blocks.
};

/* Restrictions on parsing used to signal that certain ambiguous grammar
 * features should be parsed in a certain way. */
struct ParseRestrictions
{
  bool can_be_struct_expr = true;
  /* Whether the expression was entered from a unary expression - prevents stuff
   * like struct exprs being parsed from a dereference. */
  bool entered_from_unary = false;
  bool expr_can_be_null = false;
  bool expr_can_be_stmt = false;
  bool consume_semi = true;
  /* Macro invocations that are statements can expand without a semicolon after
   * the final statement, if it's an expression statement. */
  bool allow_close_after_expr_stmt = false;
};

enum ParseSelfError
{
  SELF_PTR,
  PARSING,
  NOT_SELF,
};
// Parser implementation for gccrs.
// TODO: if updated to C++20, ManagedTokenSource would be useful as a concept
template <typename ManagedTokenSource> class Parser
{
public:
  /**
   * Consume a token
   */
  void skip_token ();

  /**
   * Consume a token, reporting an error if it isn't the next token
   *
   * @param t ID of the token to consume
   *
   * @return true if the token was next, false if it wasn't found
   */
  bool skip_token (TokenId t);

  /**
   * Consume a token, reporting an error if it isn't the next token
   *
   * @param token pointer to similar token to consume
   *
   * @return true if the token was next, false if it wasn't found
   */
  bool skip_token (const_TokenPtr token);

  /**
   * Same as `skip_token` but allows for failure without necessarily reporting
   * an error
   *
   * @param t ID of the token to consume
   *
   * @return true if the token was next, false if it wasn't found
   */
  bool maybe_skip_token (TokenId t);

  std::unique_ptr<AST::Expr>
  parse_expr (AST::AttrVec outer_attrs = AST::AttrVec (),
	      ParseRestrictions restrictions = ParseRestrictions ());

  std::unique_ptr<AST::LiteralExpr> parse_literal_expr (AST::AttrVec outer_attrs
							= AST::AttrVec ());

  std::unique_ptr<AST::BlockExpr>
  parse_block_expr (AST::AttrVec outer_attrs = AST::AttrVec (),
		    AST::LoopLabel label = AST::LoopLabel::error (),
		    location_t pratt_parsed_loc = UNKNOWN_LOCATION);

  bool is_macro_rules_def (const_TokenPtr t);
  std::unique_ptr<AST::Item> parse_item (bool called_from_statement);
  std::unique_ptr<AST::Pattern> parse_pattern ();
  std::unique_ptr<AST::Pattern> parse_pattern_no_alt ();

  /**
   * Parse a statement
   *
   * Statement : ';'
   *    | Item
   *    | LetStatement
   *    | ExpressionStatement
   *    | MacroInvocationSemi
   */
  std::unique_ptr<AST::Stmt> parse_stmt (ParseRestrictions restrictions
					 = ParseRestrictions ());
  std::unique_ptr<AST::Type> parse_type (bool save_errors = true);
  std::unique_ptr<AST::ExternalItem> parse_external_item ();
  std::unique_ptr<AST::AssociatedItem> parse_trait_item ();
  std::unique_ptr<AST::AssociatedItem> parse_inherent_impl_item ();
  std::unique_ptr<AST::AssociatedItem> parse_trait_impl_item ();
  AST::PathInExpression parse_path_in_expression ();
  std::vector<std::unique_ptr<AST::LifetimeParam>> parse_lifetime_params ();
  AST::Visibility parse_visibility ();
  std::unique_ptr<AST::IdentifierPattern> parse_identifier_pattern ();
  std::unique_ptr<AST::Token> parse_identifier_or_keyword_token ();
  std::unique_ptr<AST::TokenTree> parse_token_tree ();
  std::tuple<AST::SimplePath, std::unique_ptr<AST::AttrInput>, location_t>
  parse_attribute_body ();
  AST::AttrVec parse_inner_attributes ();
  std::unique_ptr<AST::MacroInvocation>
  parse_macro_invocation (AST::AttrVec outer_attrs);

private:
  void skip_after_semicolon ();
  void skip_after_end ();
  void skip_after_end_block ();
  void skip_after_next_block ();
  void skip_after_end_attribute ();

  const_TokenPtr expect_token (TokenId t);
  const_TokenPtr expect_token (const_TokenPtr token_expect);
  void unexpected_token (const_TokenPtr t);
  bool skip_generics_right_angle ();

  void parse_statement_seq (bool (Parser::*done) ());

  // AST-related stuff - maybe move or something?
  AST::Attribute parse_inner_attribute ();
  AST::AttrVec parse_outer_attributes ();
  AST::Attribute parse_outer_attribute ();
  std::unique_ptr<AST::AttrInput> parse_attr_input ();
  std::tuple<AST::SimplePath, std::unique_ptr<AST::AttrInput>, location_t>
  parse_doc_comment ();

  // Path-related
  AST::SimplePath parse_simple_path ();
  AST::SimplePathSegment parse_simple_path_segment ();
  AST::TypePath parse_type_path ();
  std::unique_ptr<AST::TypePathSegment> parse_type_path_segment ();
  AST::PathIdentSegment parse_path_ident_segment ();
  AST::GenericArg parse_generic_arg ();
  AST::GenericArgs parse_path_generic_args ();
  AST::GenericArgsBinding parse_generic_args_binding ();
  AST::TypePathFunction parse_type_path_function (location_t locus);
  AST::PathExprSegment parse_path_expr_segment ();
  AST::QualifiedPathInExpression
  // When given a pratt_parsed_loc, use it as the location of the
  // first token parsed in the expression (the parsing of that first
  // token should be skipped).
  parse_qualified_path_in_expression (location_t pratt_parsed_loc
				      = UNKNOWN_LOCATION);
  AST::QualifiedPathType parse_qualified_path_type (location_t pratt_parsed_loc
						    = UNKNOWN_LOCATION);
  AST::QualifiedPathInType parse_qualified_path_in_type ();

  // Token tree or macro related
  AST::DelimTokenTree parse_delim_token_tree ();
  std::unique_ptr<AST::MacroRulesDefinition>
  parse_macro_rules_def (AST::AttrVec outer_attrs);
  std::unique_ptr<AST::MacroRulesDefinition>
  parse_decl_macro_def (AST::Visibility vis, AST::AttrVec outer_attrs);
  std::unique_ptr<AST::MacroInvocation>
  parse_macro_invocation_semi (AST::AttrVec outer_attrs);
  AST::MacroRule parse_macro_rule ();
  AST::MacroMatcher parse_macro_matcher ();
  std::unique_ptr<AST::MacroMatch> parse_macro_match ();
  std::unique_ptr<AST::MacroMatchFragment> parse_macro_match_fragment ();
  std::unique_ptr<AST::MacroMatchRepetition> parse_macro_match_repetition ();

  // Top-level item-related
  std::unique_ptr<AST::VisItem> parse_vis_item (AST::AttrVec outer_attrs);

  // VisItem subclass-related
  std::unique_ptr<AST::Module> parse_module (AST::Visibility vis,
					     AST::AttrVec outer_attrs);
  std::unique_ptr<AST::ExternCrate>
  parse_extern_crate (AST::Visibility vis, AST::AttrVec outer_attrs);
  std::unique_ptr<AST::UseDeclaration>
  parse_use_decl (AST::Visibility vis, AST::AttrVec outer_attrs);
  std::unique_ptr<AST::UseTree> parse_use_tree ();
  std::unique_ptr<AST::Function> parse_function (AST::Visibility vis,
						 AST::AttrVec outer_attrs);
  AST::FunctionQualifiers parse_function_qualifiers ();
  std::vector<std::unique_ptr<AST::GenericParam>>
  parse_generic_params_in_angles ();
  template <typename EndTokenPred>
  std::vector<std::unique_ptr<AST::GenericParam>>
  parse_generic_params (EndTokenPred is_end_token);
  template <typename EndTokenPred>
  std::unique_ptr<AST::GenericParam>
  parse_generic_param (EndTokenPred is_end_token);

  template <typename EndTokenPred>
  std::vector<std::unique_ptr<AST::LifetimeParam>>
  parse_lifetime_params (EndTokenPred is_end_token);
  std::vector<AST::LifetimeParam> parse_lifetime_params_objs ();
  template <typename EndTokenPred>
  std::vector<AST::LifetimeParam>
  parse_lifetime_params_objs (EndTokenPred is_end_token);
  template <typename ParseFunction, typename EndTokenPred>
  auto parse_non_ptr_sequence (
    ParseFunction parsing_function, EndTokenPred is_end_token,
    std::string error_msg = "failed to parse generic param in generic params")
    -> std::vector<decltype (parsing_function ())>;
  AST::LifetimeParam parse_lifetime_param ();
  std::vector<std::unique_ptr<AST::TypeParam>> parse_type_params ();
  template <typename EndTokenPred>
  std::vector<std::unique_ptr<AST::TypeParam>>
  parse_type_params (EndTokenPred is_end_token);
  std::unique_ptr<AST::TypeParam> parse_type_param ();
  template <typename EndTokenPred>
  std::vector<std::unique_ptr<AST::Param>>
  parse_function_params (EndTokenPred is_end_token);
  std::unique_ptr<AST::Param> parse_function_param ();
  std::unique_ptr<AST::Type> parse_function_return_type ();
  AST::WhereClause parse_where_clause ();
  std::unique_ptr<AST::WhereClauseItem> parse_where_clause_item (
    const std::vector<AST::LifetimeParam> &global_for_lifetimes);
  std::unique_ptr<AST::LifetimeWhereClauseItem>
  parse_lifetime_where_clause_item ();
  std::unique_ptr<AST::TypeBoundWhereClauseItem>
  parse_type_bound_where_clause_item (
    const std::vector<AST::LifetimeParam> &global_for_lifetimes);
  std::vector<AST::LifetimeParam> parse_for_lifetimes ();
  template <typename EndTokenPred>
  std::vector<std::unique_ptr<AST::TypeParamBound>>
  parse_type_param_bounds (EndTokenPred is_end_token);
  std::vector<std::unique_ptr<AST::TypeParamBound>> parse_type_param_bounds ();
  std::unique_ptr<AST::TypeParamBound> parse_type_param_bound ();
  std::unique_ptr<AST::TraitBound> parse_trait_bound ();
  std::vector<AST::Lifetime> parse_lifetime_bounds ();
  template <typename EndTokenPred>
  std::vector<AST::Lifetime> parse_lifetime_bounds (EndTokenPred is_end_token);
  AST::Lifetime parse_lifetime (bool allow_elided);
  AST::Lifetime lifetime_from_token (const_TokenPtr tok);
  std::unique_ptr<AST::ExternalTypeItem>
  parse_external_type_item (AST::Visibility vis, AST::AttrVec outer_attrs);
  std::unique_ptr<AST::ExternalFunctionItem>
  parse_external_function_item (AST::Visibility vis, AST::AttrVec outer_attrs);
  AST::NamedFunctionParam parse_named_function_param ();
  template <typename EndTokenPred>
  std::vector<AST::NamedFunctionParam>
  parse_named_function_params (EndTokenPred is_end_token);

  std::unique_ptr<AST::TypeAlias> parse_type_alias (AST::Visibility vis,
						    AST::AttrVec outer_attrs);
  std::unique_ptr<AST::Struct> parse_struct (AST::Visibility vis,
					     AST::AttrVec outer_attrs);
  std::vector<AST::StructField> parse_struct_fields ();
  template <typename EndTokenPred>
  std::vector<AST::StructField> parse_struct_fields (EndTokenPred is_end_token);
  AST::StructField parse_struct_field ();
  std::vector<AST::TupleField> parse_tuple_fields ();
  AST::TupleField parse_tuple_field ();
  std::unique_ptr<AST::Enum> parse_enum (AST::Visibility vis,
					 AST::AttrVec outer_attrs);
  std::vector<std::unique_ptr<AST::EnumItem>> parse_enum_items ();
  template <typename EndTokenPred>
  std::vector<std::unique_ptr<AST::EnumItem>>
  parse_enum_items (EndTokenPred is_end_token);
  std::unique_ptr<AST::EnumItem> parse_enum_item ();
  std::unique_ptr<AST::Union> parse_union (AST::Visibility vis,
					   AST::AttrVec outer_attrs);
  std::unique_ptr<AST::ConstantItem>
  parse_const_item (AST::Visibility vis, AST::AttrVec outer_attrs);
  std::unique_ptr<AST::StaticItem> parse_static_item (AST::Visibility vis,
						      AST::AttrVec outer_attrs);
  std::unique_ptr<AST::Trait> parse_trait (AST::Visibility vis,
					   AST::AttrVec outer_attrs);
  std::unique_ptr<AST::TraitItemType>
  parse_trait_type (AST::AttrVec outer_attrs, AST::Visibility);
  std::unique_ptr<AST::TraitItemConst>
  parse_trait_const (AST::AttrVec outer_attrs);

  tl::expected<std::unique_ptr<AST::Param>, ParseSelfError> parse_self_param ();

  std::unique_ptr<AST::Impl> parse_impl (AST::Visibility vis,
					 AST::AttrVec outer_attrs);
  std::unique_ptr<AST::AssociatedItem>
  parse_inherent_impl_function_or_method (AST::Visibility vis,
					  AST::AttrVec outer_attrs);
  std::unique_ptr<AST::AssociatedItem>
  parse_trait_impl_function_or_method (AST::Visibility vis,
				       AST::AttrVec outer_attrs);
  std::unique_ptr<AST::ExternBlock>
  parse_extern_block (AST::Visibility vis, AST::AttrVec outer_attrs);
  std::unique_ptr<AST::Function> parse_method ();
  std::unique_ptr<AST::Function> parse_async_item (AST::Visibility vis,
						   AST::AttrVec outer_attrs);

  // Expression-related (Pratt parsed)
  std::unique_ptr<AST::Expr>
  parse_expr (int right_binding_power,
	      AST::AttrVec outer_attrs = AST::AttrVec (),
	      ParseRestrictions restrictions = ParseRestrictions ());
  std::unique_ptr<AST::Expr>
  null_denotation (const_TokenPtr t, AST::AttrVec outer_attrs = AST::AttrVec (),
		   ParseRestrictions restrictions = ParseRestrictions ());
  std::unique_ptr<AST::Expr>
  null_denotation_path (AST::PathInExpression path, AST::AttrVec outer_attrs,
			ParseRestrictions restrictions = ParseRestrictions ());
  std::unique_ptr<AST::Expr>
  null_denotation_not_path (const_TokenPtr t, AST::AttrVec outer_attrs,
			    ParseRestrictions restrictions
			    = ParseRestrictions ());
  std::unique_ptr<AST::Expr>
  left_denotations (std::unique_ptr<AST::Expr> null_denotation,
		    int right_binding_power, AST::AttrVec outer_attrs,
		    ParseRestrictions restrictions = ParseRestrictions ());
  std::unique_ptr<AST::Expr>
  left_denotation (const_TokenPtr t, std::unique_ptr<AST::Expr> left,
		   AST::AttrVec outer_attrs = AST::AttrVec (),
		   ParseRestrictions restrictions = ParseRestrictions ());
  std::unique_ptr<AST::ArithmeticOrLogicalExpr>
  parse_arithmetic_or_logical_expr (
    const_TokenPtr tok, std::unique_ptr<AST::Expr> left,
    AST::AttrVec outer_attrs, AST::ArithmeticOrLogicalExpr::ExprType expr_type,
    ParseRestrictions restrictions = ParseRestrictions ());
  std::unique_ptr<AST::ArithmeticOrLogicalExpr>
  parse_binary_plus_expr (const_TokenPtr tok, std::unique_ptr<AST::Expr> left,
			  AST::AttrVec outer_attrs,
			  ParseRestrictions restrictions
			  = ParseRestrictions ());
  std::unique_ptr<AST::ArithmeticOrLogicalExpr>
  parse_binary_minus_expr (const_TokenPtr tok, std::unique_ptr<AST::Expr> left,
			   AST::AttrVec outer_attrs,
			   ParseRestrictions restrictions
			   = ParseRestrictions ());
  std::unique_ptr<AST::ArithmeticOrLogicalExpr>
  parse_binary_mult_expr (const_TokenPtr tok, std::unique_ptr<AST::Expr> left,
			  AST::AttrVec outer_attrs,
			  ParseRestrictions restrictions
			  = ParseRestrictions ());
  std::unique_ptr<AST::ArithmeticOrLogicalExpr>
  parse_binary_div_expr (const_TokenPtr tok, std::unique_ptr<AST::Expr> left,
			 AST::AttrVec outer_attrs,
			 ParseRestrictions restrictions = ParseRestrictions ());
  std::unique_ptr<AST::ArithmeticOrLogicalExpr>
  parse_binary_mod_expr (const_TokenPtr tok, std::unique_ptr<AST::Expr> left,
			 AST::AttrVec outer_attrs,
			 ParseRestrictions restrictions = ParseRestrictions ());
  std::unique_ptr<AST::ArithmeticOrLogicalExpr>
  parse_bitwise_and_expr (const_TokenPtr tok, std::unique_ptr<AST::Expr> left,
			  AST::AttrVec outer_attrs,
			  ParseRestrictions restrictions
			  = ParseRestrictions ());
  std::unique_ptr<AST::ArithmeticOrLogicalExpr>
  parse_bitwise_or_expr (const_TokenPtr tok, std::unique_ptr<AST::Expr> left,
			 AST::AttrVec outer_attrs,
			 ParseRestrictions restrictions = ParseRestrictions ());
  std::unique_ptr<AST::ArithmeticOrLogicalExpr>
  parse_bitwise_xor_expr (const_TokenPtr tok, std::unique_ptr<AST::Expr> left,
			  AST::AttrVec outer_attrs,
			  ParseRestrictions restrictions
			  = ParseRestrictions ());
  std::unique_ptr<AST::ArithmeticOrLogicalExpr>
  parse_left_shift_expr (const_TokenPtr tok, std::unique_ptr<AST::Expr> left,
			 AST::AttrVec outer_attrs,
			 ParseRestrictions restrictions = ParseRestrictions ());
  std::unique_ptr<AST::ArithmeticOrLogicalExpr>
  parse_right_shift_expr (const_TokenPtr tok, std::unique_ptr<AST::Expr> left,
			  AST::AttrVec outer_attrs,
			  ParseRestrictions restrictions
			  = ParseRestrictions ());
  std::unique_ptr<AST::ComparisonExpr>
  parse_comparison_expr (const_TokenPtr tok, std::unique_ptr<AST::Expr> left,
			 AST::AttrVec outer_attrs,
			 AST::ComparisonExpr::ExprType expr_type,
			 ParseRestrictions restrictions = ParseRestrictions ());
  std::unique_ptr<AST::ComparisonExpr>
  parse_binary_equal_expr (const_TokenPtr tok, std::unique_ptr<AST::Expr> left,
			   AST::AttrVec outer_attrs,
			   ParseRestrictions restrictions
			   = ParseRestrictions ());
  std::unique_ptr<AST::ComparisonExpr> parse_binary_not_equal_expr (
    const_TokenPtr tok, std::unique_ptr<AST::Expr> left,
    AST::AttrVec outer_attrs,
    ParseRestrictions restrictions = ParseRestrictions ());
  std::unique_ptr<AST::ComparisonExpr> parse_binary_greater_than_expr (
    const_TokenPtr tok, std::unique_ptr<AST::Expr> left,
    AST::AttrVec outer_attrs,
    ParseRestrictions restrictions = ParseRestrictions ());
  std::unique_ptr<AST::ComparisonExpr> parse_binary_less_than_expr (
    const_TokenPtr tok, std::unique_ptr<AST::Expr> left,
    AST::AttrVec outer_attrs,
    ParseRestrictions restrictions = ParseRestrictions ());
  std::unique_ptr<AST::ComparisonExpr> parse_binary_greater_equal_expr (
    const_TokenPtr tok, std::unique_ptr<AST::Expr> left,
    AST::AttrVec outer_attrs,
    ParseRestrictions restrictions = ParseRestrictions ());
  std::unique_ptr<AST::ComparisonExpr> parse_binary_less_equal_expr (
    const_TokenPtr tok, std::unique_ptr<AST::Expr> left,
    AST::AttrVec outer_attrs,
    ParseRestrictions restrictions = ParseRestrictions ());
  std::unique_ptr<AST::LazyBooleanExpr>
  parse_lazy_or_expr (const_TokenPtr tok, std::unique_ptr<AST::Expr> left,
		      AST::AttrVec outer_attrs,
		      ParseRestrictions restrictions = ParseRestrictions ());
  std::unique_ptr<AST::LazyBooleanExpr>
  parse_lazy_and_expr (const_TokenPtr tok, std::unique_ptr<AST::Expr> left,
		       AST::AttrVec outer_attrs,
		       ParseRestrictions restrictions = ParseRestrictions ());
  std::unique_ptr<AST::TypeCastExpr>
  parse_type_cast_expr (const_TokenPtr tok,
			std::unique_ptr<AST::Expr> expr_to_cast,
			AST::AttrVec outer_attrs,
			ParseRestrictions restrictions = ParseRestrictions ());
  std::unique_ptr<AST::AssignmentExpr>
  parse_assig_expr (const_TokenPtr tok, std::unique_ptr<AST::Expr> left,
		    AST::AttrVec outer_attrs,
		    ParseRestrictions restrictions = ParseRestrictions ());
  std::unique_ptr<AST::CompoundAssignmentExpr> parse_compound_assignment_expr (
    const_TokenPtr tok, std::unique_ptr<AST::Expr> left,
    AST::AttrVec outer_attrs, AST::CompoundAssignmentExpr::ExprType expr_type,
    ParseRestrictions restrictions = ParseRestrictions ());
  std::unique_ptr<AST::CompoundAssignmentExpr>
  parse_plus_assig_expr (const_TokenPtr tok, std::unique_ptr<AST::Expr> left,
			 AST::AttrVec outer_attrs,
			 ParseRestrictions restrictions = ParseRestrictions ());
  std::unique_ptr<AST::CompoundAssignmentExpr>
  parse_minus_assig_expr (const_TokenPtr tok, std::unique_ptr<AST::Expr> left,
			  AST::AttrVec outer_attrs,
			  ParseRestrictions restrictions
			  = ParseRestrictions ());
  std::unique_ptr<AST::CompoundAssignmentExpr>
  parse_mult_assig_expr (const_TokenPtr tok, std::unique_ptr<AST::Expr> left,
			 AST::AttrVec outer_attrs,
			 ParseRestrictions restrictions = ParseRestrictions ());
  std::unique_ptr<AST::CompoundAssignmentExpr>
  parse_div_assig_expr (const_TokenPtr tok, std::unique_ptr<AST::Expr> left,
			AST::AttrVec outer_attrs,
			ParseRestrictions restrictions = ParseRestrictions ());
  std::unique_ptr<AST::CompoundAssignmentExpr>
  parse_mod_assig_expr (const_TokenPtr tok, std::unique_ptr<AST::Expr> left,
			AST::AttrVec outer_attrs,
			ParseRestrictions restrictions = ParseRestrictions ());
  std::unique_ptr<AST::CompoundAssignmentExpr>
  parse_and_assig_expr (const_TokenPtr tok, std::unique_ptr<AST::Expr> left,
			AST::AttrVec outer_attrs,
			ParseRestrictions restrictions = ParseRestrictions ());
  std::unique_ptr<AST::CompoundAssignmentExpr>
  parse_or_assig_expr (const_TokenPtr tok, std::unique_ptr<AST::Expr> left,
		       AST::AttrVec outer_attrs,
		       ParseRestrictions restrictions = ParseRestrictions ());
  std::unique_ptr<AST::CompoundAssignmentExpr>
  parse_xor_assig_expr (const_TokenPtr tok, std::unique_ptr<AST::Expr> left,
			AST::AttrVec outer_attrs,
			ParseRestrictions restrictions = ParseRestrictions ());
  std::unique_ptr<AST::CompoundAssignmentExpr> parse_left_shift_assig_expr (
    const_TokenPtr tok, std::unique_ptr<AST::Expr> left,
    AST::AttrVec outer_attrs,
    ParseRestrictions restrictions = ParseRestrictions ());
  std::unique_ptr<AST::CompoundAssignmentExpr> parse_right_shift_assig_expr (
    const_TokenPtr tok, std::unique_ptr<AST::Expr> left,
    AST::AttrVec outer_attrs,
    ParseRestrictions restrictions = ParseRestrictions ());
  std::unique_ptr<AST::AwaitExpr>
  parse_await_expr (const_TokenPtr tok,
		    std::unique_ptr<AST::Expr> expr_to_await,
		    AST::AttrVec outer_attrs);
  std::unique_ptr<AST::MethodCallExpr> parse_method_call_expr (
    const_TokenPtr tok, std::unique_ptr<AST::Expr> receiver_expr,
    AST::AttrVec outer_attrs,
    ParseRestrictions restrictions = ParseRestrictions ());
  std::unique_ptr<AST::CallExpr> parse_function_call_expr (
    const_TokenPtr tok, std::unique_ptr<AST::Expr> function_expr,
    AST::AttrVec outer_attrs,
    ParseRestrictions restrictions = ParseRestrictions ());
  std::unique_ptr<AST::RangeExpr> parse_led_range_exclusive_expr (
    const_TokenPtr tok, std::unique_ptr<AST::Expr> left,
    AST::AttrVec outer_attrs,
    ParseRestrictions restrictions = ParseRestrictions ());
  std::unique_ptr<AST::RangeExpr>
  parse_nud_range_exclusive_expr (const_TokenPtr tok, AST::AttrVec outer_attrs);
  std::unique_ptr<AST::RangeFromToInclExpr> parse_range_inclusive_expr (
    const_TokenPtr tok, std::unique_ptr<AST::Expr> left,
    AST::AttrVec outer_attrs,
    ParseRestrictions restrictions = ParseRestrictions ());
  std::unique_ptr<AST::RangeToInclExpr>
  parse_range_to_inclusive_expr (const_TokenPtr tok, AST::AttrVec outer_attrs);
  std::unique_ptr<AST::TupleIndexExpr> parse_tuple_index_expr (
    const_TokenPtr tok, std::unique_ptr<AST::Expr> tuple_expr,
    AST::AttrVec outer_attrs,
    ParseRestrictions restrictions = ParseRestrictions ());
  std::unique_ptr<AST::FieldAccessExpr> parse_field_access_expr (
    const_TokenPtr tok, std::unique_ptr<AST::Expr> struct_expr,
    AST::AttrVec outer_attrs,
    ParseRestrictions restrictions = ParseRestrictions ());
  std::unique_ptr<AST::ArrayIndexExpr>
  parse_index_expr (const_TokenPtr tok, std::unique_ptr<AST::Expr> array_expr,
		    AST::AttrVec outer_attrs,
		    ParseRestrictions restrictions = ParseRestrictions ());
  std::unique_ptr<AST::MacroInvocation> parse_macro_invocation_partial (
    AST::PathInExpression path, AST::AttrVec outer_attrs,
    ParseRestrictions restrictions = ParseRestrictions ());
  std::unique_ptr<AST::StructExprStruct>
  parse_struct_expr_struct_partial (AST::PathInExpression path,
				    AST::AttrVec outer_attrs);
  std::unique_ptr<AST::CallExpr>
  parse_struct_expr_tuple_partial (AST::PathInExpression path,
				   AST::AttrVec outer_attrs);
  AST::PathInExpression parse_path_in_expression_pratt (const_TokenPtr tok);
  std::unique_ptr<AST::ClosureExpr>
  parse_closure_expr_pratt (const_TokenPtr tok,
			    AST::AttrVec outer_attrs = AST::AttrVec ());
  std::unique_ptr<AST::TupleIndexExpr> parse_tuple_index_expr_float (
    const_TokenPtr tok, std::unique_ptr<AST::Expr> tuple_expr,
    AST::AttrVec outer_attrs,
    ParseRestrictions restrictions = ParseRestrictions ());

  // When given a pratt_parsed_loc, use it as the location of the
  // first token parsed in the expression (the parsing of that first
  // token should be skipped).
  std::unique_ptr<AST::IfExpr>
  parse_if_expr (AST::AttrVec outer_attrs = AST::AttrVec (),
		 location_t pratt_parsed_loc = UNKNOWN_LOCATION);
  std::unique_ptr<AST::IfLetExpr>
  parse_if_let_expr (AST::AttrVec outer_attrs = AST::AttrVec (),
		     location_t pratt_parsed_loc = UNKNOWN_LOCATION);
  std::unique_ptr<AST::LoopExpr>
  parse_loop_expr (AST::AttrVec outer_attrs = AST::AttrVec (),
		   AST::LoopLabel label = AST::LoopLabel::error (),
		   location_t pratt_parsed_loc = UNKNOWN_LOCATION);
  std::unique_ptr<AST::WhileLoopExpr>
  parse_while_loop_expr (AST::AttrVec outer_attrs = AST::AttrVec (),
			 AST::LoopLabel label = AST::LoopLabel::error (),
			 location_t pratt_parsed_loc = UNKNOWN_LOCATION);
  std::unique_ptr<AST::WhileLetLoopExpr>
  parse_while_let_loop_expr (AST::AttrVec outer_attrs = AST::AttrVec (),
			     AST::LoopLabel label = AST::LoopLabel::error ());
  std::unique_ptr<AST::ForLoopExpr>
  parse_for_loop_expr (AST::AttrVec outer_attrs = AST::AttrVec (),
		       AST::LoopLabel label = AST::LoopLabel::error ());
  std::unique_ptr<AST::MatchExpr>
  parse_match_expr (AST::AttrVec outer_attrs = AST::AttrVec (),
		    location_t pratt_parsed_loc = UNKNOWN_LOCATION);
  AST::MatchArm parse_match_arm ();
  std::vector<std::unique_ptr<AST::Pattern>>
  parse_match_arm_patterns (TokenId end_token_id);
  std::unique_ptr<AST::Expr> parse_labelled_loop_expr (const_TokenPtr tok,
						       AST::AttrVec outer_attrs
						       = AST::AttrVec ());
  AST::LoopLabel parse_loop_label (const_TokenPtr tok);
  std::unique_ptr<AST::AsyncBlockExpr>
  parse_async_block_expr (AST::AttrVec outer_attrs = AST::AttrVec ());
  std::unique_ptr<AST::GroupedExpr> parse_grouped_expr (AST::AttrVec outer_attrs
							= AST::AttrVec ());
  std::unique_ptr<AST::ClosureExpr> parse_closure_expr (AST::AttrVec outer_attrs
							= AST::AttrVec ());
  AST::ClosureParam parse_closure_param ();

  // When given a pratt_parsed_loc, use it as the location of the
  // first token parsed in the expression (the parsing of that first
  // token should be skipped).
  std::unique_ptr<AST::ReturnExpr>
  parse_return_expr (AST::AttrVec outer_attrs = AST::AttrVec (),
		     location_t pratt_parsed_loc = UNKNOWN_LOCATION);
  std::unique_ptr<AST::BreakExpr>
  parse_break_expr (AST::AttrVec outer_attrs = AST::AttrVec (),
		    location_t pratt_parsed_loc = UNKNOWN_LOCATION);
  std::unique_ptr<AST::ContinueExpr>
  parse_continue_expr (AST::AttrVec outer_attrs = AST::AttrVec (),
		       location_t pratt_parsed_loc = UNKNOWN_LOCATION);
  std::unique_ptr<AST::UnsafeBlockExpr>
  parse_unsafe_block_expr (AST::AttrVec outer_attrs = AST::AttrVec (),
			   location_t pratt_parsed_loc = UNKNOWN_LOCATION);
  std::unique_ptr<AST::ArrayExpr>
  parse_array_expr (AST::AttrVec outer_attrs = AST::AttrVec (),
		    location_t pratt_parsed_loc = UNKNOWN_LOCATION);
  std::unique_ptr<AST::ExprWithoutBlock>
  parse_grouped_or_tuple_expr (AST::AttrVec outer_attrs = AST::AttrVec (),
			       location_t pratt_parsed_loc = UNKNOWN_LOCATION);
  std::unique_ptr<AST::StructExprField> parse_struct_expr_field ();
  bool will_be_expr_with_block ();

  // Type-related
  std::unique_ptr<AST::TypeNoBounds> parse_type_no_bounds ();
  std::unique_ptr<AST::TypeNoBounds> parse_slice_or_array_type ();
  std::unique_ptr<AST::RawPointerType> parse_raw_pointer_type ();
  std::unique_ptr<AST::ReferenceType>
  parse_reference_type_inner (location_t locus);
  std::unique_ptr<AST::ReferenceType> parse_reference_type ();
  std::unique_ptr<AST::BareFunctionType>
  parse_bare_function_type (std::vector<AST::LifetimeParam> for_lifetimes);
  std::unique_ptr<AST::Type> parse_paren_prefixed_type ();
  std::unique_ptr<AST::TypeNoBounds> parse_paren_prefixed_type_no_bounds ();
  std::unique_ptr<AST::Type> parse_for_prefixed_type ();
  AST::MaybeNamedParam parse_maybe_named_param (AST::AttrVec outer_attrs);

  // Statement-related

  /**
   *Parse a let-statement
   * LetStatement :
   * 	OuterAttribute*
   * 		'let' PatternNoTopAlt ( ':' Type )? ('=' Expression )? ';'
   *
   * @param allow_no_semi Allow parsing a let-statement without expecting a
   * 		semicolon to follow it
   */
  std::unique_ptr<AST::LetStmt> parse_let_stmt (AST::AttrVec outer_attrs,
						ParseRestrictions restrictions
						= ParseRestrictions ());
  std::unique_ptr<AST::Stmt> parse_expr_stmt (AST::AttrVec outer_attrs,
					      ParseRestrictions restrictions
					      = ParseRestrictions ());
  ExprOrStmt parse_stmt_or_expr ();

  // Pattern-related
  std::unique_ptr<AST::Pattern> parse_literal_or_range_pattern ();
  std::unique_ptr<AST::RangePatternBound> parse_range_pattern_bound ();
  std::unique_ptr<AST::ReferencePattern> parse_reference_pattern ();
  std::unique_ptr<AST::Pattern> parse_grouped_or_tuple_pattern ();
  std::unique_ptr<AST::SlicePattern> parse_slice_pattern ();
  std::unique_ptr<AST::Pattern> parse_ident_leading_pattern ();
  std::unique_ptr<AST::TupleStructItems> parse_tuple_struct_items ();
  AST::StructPatternElements parse_struct_pattern_elems ();
  std::unique_ptr<AST::StructPatternField> parse_struct_pattern_field ();
  std::unique_ptr<AST::StructPatternField>
  parse_struct_pattern_field_partial (AST::AttrVec outer_attrs);

  int left_binding_power (const_TokenPtr token);

  bool done_end ();
  bool done_end_or_else ();
  bool done_end_of_file ();

  void add_error (Error error) { error_table.push_back (std::move (error)); }

public:
  // Construct parser with specified "managed" token source.
  Parser (ManagedTokenSource &tokenSource) : lexer (tokenSource) {}

  // Parse items without parsing an entire crate. This function is the main
  // parsing loop of AST::Crate::parse_crate().
  std::vector<std::unique_ptr<AST::Item>> parse_items ();

  // Main entry point for parser.
  std::unique_ptr<AST::Crate> parse_crate ();

  void debug_dump_ast_output (AST::Crate &crate, std::ostream &out);

  // Returns whether any parsing errors have occurred.
  bool has_errors () const { return !error_table.empty (); }
  // Remove all parsing errors from the table
  void clear_errors () { error_table.clear (); }

  // Get a reference to the list of errors encountered
  std::vector<Error> &get_errors () { return error_table; }

  const ManagedTokenSource &get_token_source () const { return lexer; }

  const_TokenPtr peek_current_token () { return lexer.peek_token (0); }

private:
  // The token source (usually lexer) associated with the parser.
  ManagedTokenSource &lexer;
  // The error list.
  std::vector<Error> error_table;
  // The names of inline modules while parsing.
  std::vector<std::string> inline_module_stack;

  class InlineModuleStackScope
  {
  private:
    Parser &parser;

  public:
    InlineModuleStackScope (Parser &parser, std::string name) : parser (parser)
    {
      parser.inline_module_stack.emplace_back (std::move (name));
    }
    ~InlineModuleStackScope () { parser.inline_module_stack.pop_back (); }
  };
};

std::string
extract_module_path (const AST::AttrVec &inner_attrs,
		     const AST::AttrVec &outer_attrs, const std::string &name);

/**
 * Check if a MacroMatch is allowed to follow the last parsed MacroMatch.
 *
 * @param last_match Last matcher parsed before the current match
 * @param match Current matcher to check
 *
 * @return true if the follow-up is valid, false otherwise
 */
bool
is_match_compatible (const AST::MacroMatch &last_match,
		     const AST::MacroMatch &current_match);
} // namespace Rust

// as now template, include implementations of all methods
#include "rust-parse-impl.h"

#endif // RUST_PARSE_H
