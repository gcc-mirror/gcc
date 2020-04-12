#ifndef RUST_PARSE_H
#define RUST_PARSE_H

#include "rust-lex.h"
//#include "rust-tree.h"
#include "rust-scope.h"

//#include "rust-ast-containers.h"
// TODO: change, maybe?
#include "rust-ast-full.h"

namespace Rust {
    /* HACK: used to resolve the expression-or-statement problem at the end of a block by allowing
     * either to be returned (technically). Tagged union would probably take up the same amount of
     * space. */
    struct ExprOrStmt {
        ::std::unique_ptr<AST::ExprWithoutBlock> expr;
        ::std::unique_ptr<AST::Stmt> stmt;

        /* I was going to resist the urge to make this a real class and make it POD, but construction
         * in steps is too difficult. So it'll just also have a constructor. */

        // expression constructor
        ExprOrStmt(::std::unique_ptr<AST::ExprWithoutBlock> expr) : expr(::std::move(expr)) {}

        // statement constructor
        ExprOrStmt(::std::unique_ptr<AST::Stmt> stmt) : stmt(::std::move(stmt)) {}

        // Returns whether this object is in an error state.
        inline bool is_error() const {
            return (expr == NULL && stmt == NULL) || (expr != NULL && stmt != NULL);
        }

        // Returns an error state object.
        static ExprOrStmt create_error() {
            return ExprOrStmt(NULL, NULL);
        }

        ~ExprOrStmt() = default;

        // no copy constructors/assignment copy as simple object like this shouldn't require it

        // move constructors
        ExprOrStmt(ExprOrStmt&& other) = default;
        ExprOrStmt& operator=(ExprOrStmt&& other) = default;

      private:
        // private constructor only used for creating error state expr or stmt objects
        ExprOrStmt(AST::ExprWithoutBlock* expr, AST::Stmt* stmt) : expr(expr), stmt(stmt) {}

        // make this work: have a disambiguation specifically for known statements (i.e. ';' and
        // 'let'). then, have a special "parse expr or stmt" function that returns this type. inside
        // it, it parses an expression, and then determines whether to return expr or stmt via whether
        // the next token is a semicolon. should be able to disambiguate inside that function between
        // stmts with blocks and without blocks.
    };

    /* Restrictions on parsing used to signal that certain ambiguous grammar features should be parsed
     * in a certain way.*/
    struct ParseRestrictions {
        bool can_be_struct_expr = true;
        /* Whether the expression was entered from a unary expression - prevents stuff like struct
         * exprs being parsed from a dereference. */
        bool entered_from_unary = false;
    };

    // Parser implementation for gccrs.
    class Parser {
      private:
        void skip_after_semicolon();
        void skip_after_end();
        void skip_after_end_block();
        void skip_after_next_block();
        void skip_after_end_attribute();

        bool skip_token(TokenId t);
        const_TokenPtr expect_token(TokenId t);
        void unexpected_token(const_TokenPtr t);
        bool skip_generics_right_angle();

        // expression parsing
        int left_binding_power(const_TokenPtr tok);
        Tree null_denotation(const_TokenPtr tok);
        Tree left_denotation(const_TokenPtr tok, Tree left);

        Tree parse_expression(int right_binding_power);

        Tree coerce_binary_arithmetic(const_TokenPtr tok, Tree* left, Tree* right);
        bool check_logical_operands(const_TokenPtr tok, Tree left, Tree right);

        Tree get_printf_addr();
        Tree get_puts_addr();

        Tree get_scanf_addr();

        Tree build_label_decl(const char* name, location_t loc);
        Tree build_if_statement(Tree bool_expr, Tree then_part, Tree else_part);
        Tree build_while_statement(Tree bool_expr, Tree while_body);
        Tree build_for_statement(
          SymbolPtr ind_var, Tree lower_bound, Tree upper_bound, Tree for_body_stmt_list);

        const char* print_type(Tree type);

        TreeStmtList& get_current_stmt_list();

        void enter_scope();

        struct TreeSymbolMapping {
            Tree bind_expr;
            Tree block;
        };

        TreeSymbolMapping leave_scope();

        SymbolPtr query_type(const std::string& name, location_t loc);
        SymbolPtr query_variable(const std::string& name, location_t loc);
        SymbolPtr query_integer_variable(const std::string& name, location_t loc);

        void parse_statement_seq(bool (Parser::*done)());

        // AST-related stuff - maybe move or something?
        ::std::vector<AST::Attribute> parse_inner_attributes();
        AST::Attribute parse_inner_attribute();
        ::std::vector<AST::Attribute> parse_outer_attributes();
        AST::Attribute parse_outer_attribute();
        AST::Attribute parse_attribute_body();
        ::std::unique_ptr<AST::AttrInput> parse_attr_input();

        // Path-related
        AST::SimplePath parse_simple_path();
        AST::SimplePathSegment parse_simple_path_segment();
        AST::TypePath parse_type_path();
        ::std::unique_ptr<AST::TypePathSegment> parse_type_path_segment();
        AST::PathIdentSegment parse_path_ident_segment();
        AST::GenericArgs parse_path_generic_args();
        AST::GenericArgsBinding parse_generic_args_binding();
        AST::TypePathFunction parse_type_path_function();
        AST::PathInExpression parse_path_in_expression();
        AST::PathExprSegment parse_path_expr_segment();
        AST::QualifiedPathInExpression parse_qualified_path_in_expression(bool pratt_parse = false);
        AST::QualifiedPathType parse_qualified_path_type(bool pratt_parse = false);
        AST::QualifiedPathInType parse_qualified_path_in_type();

        // Token tree or macro related
        AST::DelimTokenTree parse_delim_token_tree();
        ::std::unique_ptr<AST::TokenTree> parse_token_tree();
        ::std::unique_ptr<AST::MacroRulesDefinition> parse_macro_rules_def(
          ::std::vector<AST::Attribute> outer_attrs);
        ::std::unique_ptr<AST::MacroInvocationSemi> parse_macro_invocation_semi(
          ::std::vector<AST::Attribute> outer_attrs);
        ::std::unique_ptr<AST::MacroInvocation> parse_macro_invocation(
          ::std::vector<AST::Attribute> outer_attrs);
        AST::MacroRule parse_macro_rule();
        AST::MacroMatcher parse_macro_matcher();
        ::std::unique_ptr<AST::MacroMatch> parse_macro_match();
        ::std::unique_ptr<AST::MacroMatchFragment> parse_macro_match_fragment();
        ::std::unique_ptr<AST::MacroMatchRepetition> parse_macro_match_repetition();

        // Top-level item-related
        ::std::vector< ::std::unique_ptr<AST::Item> > parse_items();
        ::std::unique_ptr<AST::Item> parse_item(bool called_from_statement);
        ::std::unique_ptr<AST::VisItem> parse_vis_item(::std::vector<AST::Attribute> outer_attrs);
        ::std::unique_ptr<AST::MacroItem> parse_macro_item(::std::vector<AST::Attribute> outer_attrs);
        AST::Visibility parse_visibility();

        // VisItem subclass-related
        ::std::unique_ptr<AST::Module> parse_module(
          AST::Visibility vis, ::std::vector<AST::Attribute> outer_attrs);
        ::std::unique_ptr<AST::ExternCrate> parse_extern_crate(
          AST::Visibility vis, ::std::vector<AST::Attribute> outer_attrs);
        ::std::unique_ptr<AST::UseDeclaration> parse_use_decl(
          AST::Visibility vis, ::std::vector<AST::Attribute> outer_attrs);
        ::std::unique_ptr<AST::UseTree> parse_use_tree();
        ::std::unique_ptr<AST::Function> parse_function(
          AST::Visibility vis, ::std::vector<AST::Attribute> outer_attrs);
        AST::FunctionQualifiers parse_function_qualifiers();
        ::std::vector< ::std::unique_ptr<AST::GenericParam> > parse_generic_params_in_angles();
        ::std::vector< ::std::unique_ptr<AST::GenericParam> > parse_generic_params();
        ::std::vector< ::std::unique_ptr<AST::LifetimeParam> > parse_lifetime_params();
        ::std::vector<AST::LifetimeParam> parse_lifetime_params_objs();
        AST::LifetimeParam parse_lifetime_param();
        ::std::vector< ::std::unique_ptr<AST::TypeParam> > parse_type_params();
        ::std::unique_ptr<AST::TypeParam> parse_type_param();
        ::std::vector<AST::FunctionParam> parse_function_params();
        AST::FunctionParam parse_function_param();
        ::std::unique_ptr<AST::Type> parse_function_return_type();
        AST::WhereClause parse_where_clause();
        ::std::unique_ptr<AST::WhereClauseItem> parse_where_clause_item();
        ::std::unique_ptr<AST::LifetimeWhereClauseItem> parse_lifetime_where_clause_item();
        ::std::unique_ptr<AST::TypeBoundWhereClauseItem> parse_type_bound_where_clause_item();
        ::std::vector<AST::LifetimeParam> parse_for_lifetimes();
        ::std::vector< ::std::unique_ptr<AST::TypeParamBound> > parse_type_param_bounds();
        ::std::unique_ptr<AST::TypeParamBound> parse_type_param_bound();
        ::std::unique_ptr<AST::TraitBound> parse_trait_bound();
        ::std::vector<AST::Lifetime> parse_lifetime_bounds();
        AST::Lifetime parse_lifetime();
        ::std::unique_ptr<AST::TypeAlias> parse_type_alias(
          AST::Visibility vis, ::std::vector<AST::Attribute> outer_attrs);
        ::std::unique_ptr<AST::Struct> parse_struct(
          AST::Visibility vis, ::std::vector<AST::Attribute> outer_attrs);
        ::std::vector<AST::StructField> parse_struct_fields();
        AST::StructField parse_struct_field();
        ::std::vector<AST::TupleField> parse_tuple_fields();
        AST::TupleField parse_tuple_field();
        ::std::unique_ptr<AST::Enum> parse_enum(
          AST::Visibility vis, ::std::vector<AST::Attribute> outer_attrs);
        ::std::vector< ::std::unique_ptr<AST::EnumItem> > parse_enum_items();
        ::std::unique_ptr<AST::EnumItem> parse_enum_item();
        ::std::unique_ptr<AST::Union> parse_union(
          AST::Visibility vis, ::std::vector<AST::Attribute> outer_attrs);
        ::std::unique_ptr<AST::ConstantItem> parse_const_item(
          AST::Visibility vis, ::std::vector<AST::Attribute> outer_attrs);
        ::std::unique_ptr<AST::StaticItem> parse_static_item(
          AST::Visibility vis, ::std::vector<AST::Attribute> outer_attrs);
        ::std::unique_ptr<AST::Trait> parse_trait(
          AST::Visibility vis, ::std::vector<AST::Attribute> outer_attrs);
        ::std::unique_ptr<AST::TraitItem> parse_trait_item();
        ::std::unique_ptr<AST::TraitItemType> parse_trait_type(
          ::std::vector<AST::Attribute> outer_attrs);
        ::std::unique_ptr<AST::TraitItemConst> parse_trait_const(
          ::std::vector<AST::Attribute> outer_attrs);
        AST::SelfParam parse_self_param();
        ::std::unique_ptr<AST::Impl> parse_impl(
          AST::Visibility vis, ::std::vector<AST::Attribute> outer_attrs);
        ::std::unique_ptr<AST::InherentImplItem> parse_inherent_impl_item();
        ::std::unique_ptr<AST::InherentImplItem> parse_inherent_impl_function_or_method(
          AST::Visibility vis, ::std::vector<AST::Attribute> outer_attrs);
        ::std::unique_ptr<AST::TraitImplItem> parse_trait_impl_item();
        ::std::unique_ptr<AST::TraitImplItem> parse_trait_impl_function_or_method(
          AST::Visibility vis, ::std::vector<AST::Attribute> outer_attrs);
        ::std::unique_ptr<AST::ExternBlock> parse_extern_block(
          AST::Visibility vis, ::std::vector<AST::Attribute> outer_attrs);
        ::std::unique_ptr<AST::ExternalItem> parse_external_item();
        AST::NamedFunctionParam parse_named_function_param();
        AST::Method parse_method();

        // Expression-related (Pratt parsed)
        ::std::unique_ptr<AST::Expr> parse_expr(
          ::std::vector<AST::Attribute> outer_attrs = ::std::vector<AST::Attribute>(),
          ParseRestrictions restrictions = ParseRestrictions());
        ::std::unique_ptr<AST::Expr> parse_expr(int right_binding_power,
          ::std::vector<AST::Attribute> outer_attrs = ::std::vector<AST::Attribute>(),
          ParseRestrictions restrictions = ParseRestrictions());
        ::std::unique_ptr<AST::Expr> null_denotation_NEW(const_TokenPtr t,
          ::std::vector<AST::Attribute> outer_attrs = ::std::vector<AST::Attribute>(),
          ParseRestrictions restrictions = ParseRestrictions());
        ::std::unique_ptr<AST::Expr> left_denotation(const_TokenPtr t,
          ::std::unique_ptr<AST::Expr> left,
          ::std::vector<AST::Attribute> outer_attrs = ::std::vector<AST::Attribute>(),
          ParseRestrictions restrictions = ParseRestrictions());
        ::std::unique_ptr<AST::ArithmeticOrLogicalExpr> parse_binary_plus_expr(const_TokenPtr tok,
          ::std::unique_ptr<AST::Expr> left, ::std::vector<AST::Attribute> outer_attrs,
          ParseRestrictions restrictions = ParseRestrictions());
        ::std::unique_ptr<AST::ArithmeticOrLogicalExpr> parse_binary_minus_expr(const_TokenPtr tok,
          ::std::unique_ptr<AST::Expr> left, ::std::vector<AST::Attribute> outer_attrs,
          ParseRestrictions restrictions = ParseRestrictions());
        ::std::unique_ptr<AST::ArithmeticOrLogicalExpr> parse_binary_mult_expr(const_TokenPtr tok,
          ::std::unique_ptr<AST::Expr> left, ::std::vector<AST::Attribute> outer_attrs,
          ParseRestrictions restrictions = ParseRestrictions());
        ::std::unique_ptr<AST::ArithmeticOrLogicalExpr> parse_binary_div_expr(const_TokenPtr tok,
          ::std::unique_ptr<AST::Expr> left, ::std::vector<AST::Attribute> outer_attrs,
          ParseRestrictions restrictions = ParseRestrictions());
        ::std::unique_ptr<AST::ArithmeticOrLogicalExpr> parse_binary_mod_expr(const_TokenPtr tok,
          ::std::unique_ptr<AST::Expr> left, ::std::vector<AST::Attribute> outer_attrs,
          ParseRestrictions restrictions = ParseRestrictions());
        ::std::unique_ptr<AST::ArithmeticOrLogicalExpr> parse_bitwise_and_expr(const_TokenPtr tok,
          ::std::unique_ptr<AST::Expr> left, ::std::vector<AST::Attribute> outer_attrs,
          ParseRestrictions restrictions = ParseRestrictions());
        ::std::unique_ptr<AST::ArithmeticOrLogicalExpr> parse_bitwise_or_expr(const_TokenPtr tok,
          ::std::unique_ptr<AST::Expr> left, ::std::vector<AST::Attribute> outer_attrs,
          ParseRestrictions restrictions = ParseRestrictions());
        ::std::unique_ptr<AST::ArithmeticOrLogicalExpr> parse_bitwise_xor_expr(const_TokenPtr tok,
          ::std::unique_ptr<AST::Expr> left, ::std::vector<AST::Attribute> outer_attrs,
          ParseRestrictions restrictions = ParseRestrictions());
        ::std::unique_ptr<AST::ArithmeticOrLogicalExpr> parse_left_shift_expr(const_TokenPtr tok,
          ::std::unique_ptr<AST::Expr> left, ::std::vector<AST::Attribute> outer_attrs,
          ParseRestrictions restrictions = ParseRestrictions());
        ::std::unique_ptr<AST::ArithmeticOrLogicalExpr> parse_right_shift_expr(const_TokenPtr tok,
          ::std::unique_ptr<AST::Expr> left, ::std::vector<AST::Attribute> outer_attrs,
          ParseRestrictions restrictions = ParseRestrictions());
        ::std::unique_ptr<AST::ComparisonExpr> parse_binary_equal_expr(const_TokenPtr tok,
          ::std::unique_ptr<AST::Expr> left, ::std::vector<AST::Attribute> outer_attrs,
          ParseRestrictions restrictions = ParseRestrictions());
        ::std::unique_ptr<AST::ComparisonExpr> parse_binary_not_equal_expr(const_TokenPtr tok,
          ::std::unique_ptr<AST::Expr> left, ::std::vector<AST::Attribute> outer_attrs,
          ParseRestrictions restrictions = ParseRestrictions());
        ::std::unique_ptr<AST::ComparisonExpr> parse_binary_greater_than_expr(const_TokenPtr tok,
          ::std::unique_ptr<AST::Expr> left, ::std::vector<AST::Attribute> outer_attrs,
          ParseRestrictions restrictions = ParseRestrictions());
        ::std::unique_ptr<AST::ComparisonExpr> parse_binary_less_than_expr(const_TokenPtr tok,
          ::std::unique_ptr<AST::Expr> left, ::std::vector<AST::Attribute> outer_attrs,
          ParseRestrictions restrictions = ParseRestrictions());
        ::std::unique_ptr<AST::ComparisonExpr> parse_binary_greater_equal_expr(const_TokenPtr tok,
          ::std::unique_ptr<AST::Expr> left, ::std::vector<AST::Attribute> outer_attrs,
          ParseRestrictions restrictions = ParseRestrictions());
        ::std::unique_ptr<AST::ComparisonExpr> parse_binary_less_equal_expr(const_TokenPtr tok,
          ::std::unique_ptr<AST::Expr> left, ::std::vector<AST::Attribute> outer_attrs,
          ParseRestrictions restrictions = ParseRestrictions());
        ::std::unique_ptr<AST::LazyBooleanExpr> parse_lazy_or_expr(const_TokenPtr tok,
          ::std::unique_ptr<AST::Expr> left, ::std::vector<AST::Attribute> outer_attrs,
          ParseRestrictions restrictions = ParseRestrictions());
        ::std::unique_ptr<AST::LazyBooleanExpr> parse_lazy_and_expr(const_TokenPtr tok,
          ::std::unique_ptr<AST::Expr> left, ::std::vector<AST::Attribute> outer_attrs,
          ParseRestrictions restrictions = ParseRestrictions());
        ::std::unique_ptr<AST::TypeCastExpr> parse_type_cast_expr(const_TokenPtr tok,
          ::std::unique_ptr<AST::Expr> expr_to_cast, ::std::vector<AST::Attribute> outer_attrs,
          ParseRestrictions restrictions = ParseRestrictions());
        ::std::unique_ptr<AST::AssignmentExpr> parse_assig_expr(const_TokenPtr tok,
          ::std::unique_ptr<AST::Expr> left, ::std::vector<AST::Attribute> outer_attrs,
          ParseRestrictions restrictions = ParseRestrictions());
        ::std::unique_ptr<AST::CompoundAssignmentExpr> parse_plus_assig_expr(const_TokenPtr tok,
          ::std::unique_ptr<AST::Expr> left, ::std::vector<AST::Attribute> outer_attrs,
          ParseRestrictions restrictions = ParseRestrictions());
        ::std::unique_ptr<AST::CompoundAssignmentExpr> parse_minus_assig_expr(const_TokenPtr tok,
          ::std::unique_ptr<AST::Expr> left, ::std::vector<AST::Attribute> outer_attrs,
          ParseRestrictions restrictions = ParseRestrictions());
        ::std::unique_ptr<AST::CompoundAssignmentExpr> parse_mult_assig_expr(const_TokenPtr tok,
          ::std::unique_ptr<AST::Expr> left, ::std::vector<AST::Attribute> outer_attrs,
          ParseRestrictions restrictions = ParseRestrictions());
        ::std::unique_ptr<AST::CompoundAssignmentExpr> parse_div_assig_expr(const_TokenPtr tok,
          ::std::unique_ptr<AST::Expr> left, ::std::vector<AST::Attribute> outer_attrs,
          ParseRestrictions restrictions = ParseRestrictions());
        ::std::unique_ptr<AST::CompoundAssignmentExpr> parse_mod_assig_expr(const_TokenPtr tok,
          ::std::unique_ptr<AST::Expr> left, ::std::vector<AST::Attribute> outer_attrs,
          ParseRestrictions restrictions = ParseRestrictions());
        ::std::unique_ptr<AST::CompoundAssignmentExpr> parse_and_assig_expr(const_TokenPtr tok,
          ::std::unique_ptr<AST::Expr> left, ::std::vector<AST::Attribute> outer_attrs,
          ParseRestrictions restrictions = ParseRestrictions());
        ::std::unique_ptr<AST::CompoundAssignmentExpr> parse_or_assig_expr(const_TokenPtr tok,
          ::std::unique_ptr<AST::Expr> left, ::std::vector<AST::Attribute> outer_attrs,
          ParseRestrictions restrictions = ParseRestrictions());
        ::std::unique_ptr<AST::CompoundAssignmentExpr> parse_xor_assig_expr(const_TokenPtr tok,
          ::std::unique_ptr<AST::Expr> left, ::std::vector<AST::Attribute> outer_attrs,
          ParseRestrictions restrictions = ParseRestrictions());
        ::std::unique_ptr<AST::CompoundAssignmentExpr> parse_left_shift_assig_expr(const_TokenPtr tok,
          ::std::unique_ptr<AST::Expr> left, ::std::vector<AST::Attribute> outer_attrs,
          ParseRestrictions restrictions = ParseRestrictions());
        ::std::unique_ptr<AST::CompoundAssignmentExpr> parse_right_shift_assig_expr(
          const_TokenPtr tok, ::std::unique_ptr<AST::Expr> left,
          ::std::vector<AST::Attribute> outer_attrs,
          ParseRestrictions restrictions = ParseRestrictions());
        ::std::unique_ptr<AST::AwaitExpr> parse_await_expr(const_TokenPtr tok,
          ::std::unique_ptr<AST::Expr> expr_to_await, ::std::vector<AST::Attribute> outer_attrs);
        ::std::unique_ptr<AST::MethodCallExpr> parse_method_call_expr(const_TokenPtr tok,
          ::std::unique_ptr<AST::Expr> receiver_expr, ::std::vector<AST::Attribute> outer_attrs,
          ParseRestrictions restrictions = ParseRestrictions());
        ::std::unique_ptr<AST::CallExpr> parse_function_call_expr(const_TokenPtr tok,
          ::std::unique_ptr<AST::Expr> function_expr, ::std::vector<AST::Attribute> outer_attrs,
          ParseRestrictions restrictions = ParseRestrictions());
        ::std::unique_ptr<AST::RangeExpr> parse_led_range_exclusive_expr(const_TokenPtr tok,
          ::std::unique_ptr<AST::Expr> left, ::std::vector<AST::Attribute> outer_attrs,
          ParseRestrictions restrictions = ParseRestrictions());
        ::std::unique_ptr<AST::RangeExpr> parse_nud_range_exclusive_expr(
          const_TokenPtr tok, ::std::vector<AST::Attribute> outer_attrs);
        ::std::unique_ptr<AST::RangeFromToInclExpr> parse_range_inclusive_expr(const_TokenPtr tok,
          ::std::unique_ptr<AST::Expr> left, ::std::vector<AST::Attribute> outer_attrs,
          ParseRestrictions restrictions = ParseRestrictions());
        ::std::unique_ptr<AST::RangeToInclExpr> parse_range_to_inclusive_expr(
          const_TokenPtr tok, ::std::vector<AST::Attribute> outer_attrs);
        ::std::unique_ptr<AST::TupleIndexExpr> parse_tuple_index_expr(const_TokenPtr tok,
          ::std::unique_ptr<AST::Expr> tuple_expr, ::std::vector<AST::Attribute> outer_attrs,
          ParseRestrictions restrictions = ParseRestrictions());
        ::std::unique_ptr<AST::FieldAccessExpr> parse_field_access_expr(const_TokenPtr tok,
          ::std::unique_ptr<AST::Expr> struct_expr, ::std::vector<AST::Attribute> outer_attrs,
          ParseRestrictions restrictions = ParseRestrictions());
        ::std::unique_ptr<AST::ArrayIndexExpr> parse_index_expr(const_TokenPtr tok,
          ::std::unique_ptr<AST::Expr> array_expr, ::std::vector<AST::Attribute> outer_attrs,
          ParseRestrictions restrictions = ParseRestrictions());
        ::std::unique_ptr<AST::MacroInvocation> parse_macro_invocation_partial(
          AST::PathInExpression path, ::std::vector<AST::Attribute> outer_attrs);
        ::std::unique_ptr<AST::StructExprStruct> parse_struct_expr_struct_partial(
          AST::PathInExpression path, ::std::vector<AST::Attribute> outer_attrs);
        ::std::unique_ptr<AST::StructExprTuple> parse_struct_expr_tuple_partial(
          AST::PathInExpression path, ::std::vector<AST::Attribute> outer_attrs);
        AST::PathInExpression parse_path_in_expression_pratt(const_TokenPtr tok);
        ::std::unique_ptr<AST::ClosureExpr> parse_closure_expr_pratt(const_TokenPtr tok,
          ::std::vector<AST::Attribute> outer_attrs = ::std::vector<AST::Attribute>());
        ::std::unique_ptr<AST::TupleIndexExpr> parse_tuple_index_expr_float(const_TokenPtr tok,
          ::std::unique_ptr<AST::Expr> tuple_expr, ::std::vector<AST::Attribute> outer_attrs,
          ParseRestrictions restrictions = ParseRestrictions());

        // Expression-related (non-Pratt parsed)
        ::std::unique_ptr<AST::ExprWithoutBlock> parse_expr_without_block(
          ::std::vector<AST::Attribute> outer_attrs = ::std::vector<AST::Attribute>());
        ::std::unique_ptr<AST::BlockExpr> parse_block_expr(
          ::std::vector<AST::Attribute> outer_attrs = ::std::vector<AST::Attribute>(),
          bool pratt_parse = false);
        ::std::unique_ptr<AST::IfExpr> parse_if_expr(
          ::std::vector<AST::Attribute> outer_attrs = ::std::vector<AST::Attribute>());
        ::std::unique_ptr<AST::IfLetExpr> parse_if_let_expr(
          ::std::vector<AST::Attribute> outer_attrs = ::std::vector<AST::Attribute>());
        ::std::unique_ptr<AST::LoopExpr> parse_loop_expr(
          ::std::vector<AST::Attribute> outer_attrs = ::std::vector<AST::Attribute>(),
          AST::LoopLabel label = AST::LoopLabel::error());
        ::std::unique_ptr<AST::WhileLoopExpr> parse_while_loop_expr(
          ::std::vector<AST::Attribute> outer_attrs = ::std::vector<AST::Attribute>(),
          AST::LoopLabel label = AST::LoopLabel::error());
        ::std::unique_ptr<AST::WhileLetLoopExpr> parse_while_let_loop_expr(
          ::std::vector<AST::Attribute> outer_attrs = ::std::vector<AST::Attribute>(),
          AST::LoopLabel label = AST::LoopLabel::error());
        ::std::unique_ptr<AST::ForLoopExpr> parse_for_loop_expr(
          ::std::vector<AST::Attribute> outer_attrs = ::std::vector<AST::Attribute>(),
          AST::LoopLabel label = AST::LoopLabel::error());
        ::std::unique_ptr<AST::MatchExpr> parse_match_expr(
          ::std::vector<AST::Attribute> outer_attrs = ::std::vector<AST::Attribute>(),
          bool pratt_parse = false);
        AST::MatchArm parse_match_arm();
        ::std::vector< ::std::unique_ptr<AST::Pattern> > parse_match_arm_patterns(
          TokenId end_token_id);
        ::std::unique_ptr<AST::BaseLoopExpr> parse_labelled_loop_expr(
          ::std::vector<AST::Attribute> outer_attrs = ::std::vector<AST::Attribute>());
        AST::LoopLabel parse_loop_label();
        ::std::unique_ptr<AST::AsyncBlockExpr> parse_async_block_expr(
          ::std::vector<AST::Attribute> outer_attrs = ::std::vector<AST::Attribute>());
        ::std::unique_ptr<AST::UnsafeBlockExpr> parse_unsafe_block_expr(
          ::std::vector<AST::Attribute> outer_attrs = ::std::vector<AST::Attribute>());
        ::std::unique_ptr<AST::GroupedExpr> parse_grouped_expr(
          ::std::vector<AST::Attribute> outer_attrs = ::std::vector<AST::Attribute>());
        ::std::unique_ptr<AST::ClosureExpr> parse_closure_expr(
          ::std::vector<AST::Attribute> outer_attrs = ::std::vector<AST::Attribute>());
        AST::ClosureParam parse_closure_param();
        ::std::unique_ptr<AST::LiteralExpr> parse_literal_expr(
          ::std::vector<AST::Attribute> outer_attrs = ::std::vector<AST::Attribute>());
        ::std::unique_ptr<AST::ReturnExpr> parse_return_expr(
          ::std::vector<AST::Attribute> outer_attrs = ::std::vector<AST::Attribute>(),
          bool pratt_parse = false);
        ::std::unique_ptr<AST::BreakExpr> parse_break_expr(
          ::std::vector<AST::Attribute> outer_attrs = ::std::vector<AST::Attribute>(),
          bool pratt_parse = false);
        ::std::unique_ptr<AST::ContinueExpr> parse_continue_expr(
          ::std::vector<AST::Attribute> outer_attrs = ::std::vector<AST::Attribute>(),
          bool pratt_parse = false);
        ::std::unique_ptr<AST::ArrayExpr> parse_array_expr(
          ::std::vector<AST::Attribute> outer_attrs = ::std::vector<AST::Attribute>(),
          bool pratt_parse = false);
        ::std::unique_ptr<AST::ExprWithoutBlock> parse_grouped_or_tuple_expr(
          ::std::vector<AST::Attribute> outer_attrs = ::std::vector<AST::Attribute>(),
          bool pratt_parse = false);
        ::std::unique_ptr<AST::StructExprField> parse_struct_expr_field();

        // Type-related
        ::std::unique_ptr<AST::Type> parse_type();
        ::std::unique_ptr<AST::TypeNoBounds> parse_type_no_bounds();
        ::std::unique_ptr<AST::TypeNoBounds> parse_slice_or_array_type();
        ::std::unique_ptr<AST::RawPointerType> parse_raw_pointer_type();
        ::std::unique_ptr<AST::ReferenceType> parse_reference_type();
        ::std::unique_ptr<AST::BareFunctionType> parse_bare_function_type(
          ::std::vector<AST::LifetimeParam> for_lifetimes);
        ::std::unique_ptr<AST::Type> parse_paren_prefixed_type();
        ::std::unique_ptr<AST::TypeNoBounds> parse_paren_prefixed_type_no_bounds();
        ::std::unique_ptr<AST::Type> parse_for_prefixed_type();
        AST::MaybeNamedParam parse_maybe_named_param();

        // Statement-related
        ::std::unique_ptr<AST::Stmt> parse_stmt();
        ::std::unique_ptr<AST::LetStmt> parse_let_stmt(::std::vector<AST::Attribute> outer_attrs);
        ::std::unique_ptr<AST::ExprStmt> parse_expr_stmt(::std::vector<AST::Attribute> outer_attrs);
        ::std::unique_ptr<AST::ExprStmtWithBlock> parse_expr_stmt_with_block(
          ::std::vector<AST::Attribute> outer_attrs);
        ::std::unique_ptr<AST::ExprStmtWithoutBlock> parse_expr_stmt_without_block(
          ::std::vector<AST::Attribute> outer_attrs);
        ExprOrStmt parse_stmt_or_expr_without_block();
        ExprOrStmt parse_macro_invocation_maybe_semi(::std::vector<AST::Attribute> outer_attrs);
        ExprOrStmt parse_path_based_stmt_or_expr(::std::vector<AST::Attribute> outer_attrs);

        // Pattern-related
        ::std::unique_ptr<AST::Pattern> parse_pattern();
        ::std::unique_ptr<AST::Pattern> parse_literal_or_range_pattern();
        ::std::unique_ptr<AST::RangePatternBound> parse_range_pattern_bound();
        ::std::unique_ptr<AST::ReferencePattern> parse_reference_pattern();
        ::std::unique_ptr<AST::Pattern> parse_grouped_or_tuple_pattern();
        ::std::unique_ptr<AST::SlicePattern> parse_slice_pattern();
        ::std::unique_ptr<AST::IdentifierPattern> parse_identifier_pattern();
        ::std::unique_ptr<AST::Pattern> parse_ident_leading_pattern();
        ::std::unique_ptr<AST::TupleStructItems> parse_tuple_struct_items();
        AST::StructPatternElements parse_struct_pattern_elems();
        ::std::unique_ptr<AST::StructPatternField> parse_struct_pattern_field();

        // void parse_crate();
        // AST::Module parse_module();
        // void parse_module_item(AST::Module module_for_items, AST::AttributeList
        // item_outer_attributes); AST::Visibility parse_visibility();

        bool done_end();
        bool done_end_or_else();
        bool done_end_of_file();

        typedef Tree (Parser::*BinaryHandler)(const_TokenPtr, Tree);
        BinaryHandler get_binary_handler(TokenId id);

      public:
        // Construct parser with specified lexer reference.
        Parser(Lexer& parLexer) : lexer(parLexer), printf_fn(), puts_fn(), scanf_fn() {}

        // (old) Main entry point for parser.
        void parse_program();

        // Main entry point for parser.
        AST::Crate parse_crate();

        Tree parse_statement();

        Tree parse_variable_declaration();
        Tree parse_type_declaration();

        // Tree parse_type();
        Tree parse_record();
        Tree parse_field_declaration(std::vector<std::string>& field_names);

        Tree parse_assignment_statement();
        Tree parse_if_statement();
        Tree parse_while_statement();
        Tree parse_for_statement();
        Tree parse_read_statement();
        Tree parse_write_statement();

        Tree parse_expression();
        Tree parse_expression_naming_variable();
        Tree parse_lhs_assignment_expression();
        Tree parse_boolean_expression();
        Tree parse_integer_expression();

        // Dumps all lexer output.
        void debug_dump_lex_output();
        void debug_dump_ast_output();

      private:
        // The lexer associated with the parser.
        Lexer& lexer;
        // The current scope.
        Scope scope;

        // The simulated "main" function inside which the entire program lies.
        tree main_fndecl;

        // Address to function declaration of printf.
        Tree printf_fn;
        // Address to function declaration of puts.
        Tree puts_fn;
        // Address to function declaration of scanf.
        Tree scanf_fn;

        // The statement stack.
        std::vector<TreeStmtList> stack_stmt_list;
        // The VAR_DECL stack.
        std::vector<TreeChain> stack_var_decl_chain;

        // The block stack.
        std::vector<BlockChain> stack_block_chain;

// x-macro list for binary operators - only defined here to be inside Parser class
#define BINARY_HANDLER_LIST                                             \
    BINARY_HANDLER(plus, PLUS)                                          \
    BINARY_HANDLER(minus, MINUS)                                        \
    BINARY_HANDLER(mult, ASTERISK)                                      \
    BINARY_HANDLER(div, DIV)                                            \
    BINARY_HANDLER(mod, PERCENT)                                        \
    BINARY_HANDLER(bitwise_and, AMP)                                    \
    BINARY_HANDLER(bitwise_or, PIPE)                                    \
    BINARY_HANDLER(bitwise_xor, CARET)                                  \
    BINARY_HANDLER(left_shift, LEFT_SHIFT)                              \
    BINARY_HANDLER(right_shift, RIGHT_SHIFT)                            \
                                                                        \
    BINARY_HANDLER(equal, EQUAL_EQUAL)                                  \
    BINARY_HANDLER(not_equal, NOT_EQUAL)                                \
    BINARY_HANDLER(smaller_than, LEFT_ANGLE)                            \
    BINARY_HANDLER(smaller_equal, LESS_OR_EQUAL)                        \
    BINARY_HANDLER(greater_than, RIGHT_ANGLE)                           \
    BINARY_HANDLER(greater_equal, GREATER_OR_EQUAL)                     \
                                                                        \
    BINARY_HANDLER(logical_and, LOGICAL_AND)                            \
    BINARY_HANDLER(logical_or, OR)                                      \
                                                                        \
    BINARY_HANDLER(as_cast, AS)                                         \
    /* should this really be an operator? */                            \
                                                                        \
    BINARY_HANDLER(array_index, LEFT_SQUARE)                            \
                                                                        \
    BINARY_HANDLER(field_ref, DOT)                                      \
    /*BINARY_HANDLER(method_call, DOT)*/                                \
    BINARY_HANDLER(error_propagation, QUESTION_MARK)                    \
    /* not a binary operator, technically, but still left denotation */ \
    BINARY_HANDLER(assignment_expr, EQUAL)                              \
    /* should this really be an operator? or a binary one? */           \
    /* if it should, also add all operation-assign below:*/             \
    BINARY_HANDLER(plus_assig, PLUS_EQ)                                 \
    BINARY_HANDLER(minus_assig, MINUS_EQ)                               \
    BINARY_HANDLER(mult_assig, ASTERISK_EQ)                             \
    BINARY_HANDLER(div_assig, DIV_EQ)                                   \
    BINARY_HANDLER(mod_assig, PERCENT_EQ)                               \
    BINARY_HANDLER(bitwise_and_assig, AMP_EQ)                           \
    BINARY_HANDLER(bitwise_or_assig, PIPE_EQ)                           \
    BINARY_HANDLER(bitwise_xor_assig, CARET_EQ)                         \
    BINARY_HANDLER(left_shift_assig, LEFT_SHIFT_EQ)                     \
    BINARY_HANDLER(right_shift_assig, RIGHT_SHIFT_EQ)                   \
                                                                        \
    BINARY_HANDLER(range_exclusive, DOT_DOT)                            \
    BINARY_HANDLER(range_inclusive, DOT_DOT_EQ)                         \
                                                                        \
    BINARY_HANDLER(path, SCOPE_RESOLUTION)                              \
                                                                        \
    BINARY_HANDLER(return, RETURN_TOK)                                  \
    BINARY_HANDLER(break, BREAK)                                        \
    BINARY_HANDLER(closure, MOVE)

// create declarations for binary op handling
#define BINARY_HANDLER(name, _) Tree binary_##name(const_TokenPtr tok, Tree left);
        BINARY_HANDLER_LIST
#undef BINARY_HANDLER
    };
}

#endif // RUST_PARSE_H