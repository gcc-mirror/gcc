#ifndef RUST_PARSE_H
#define RUST_PARSE_H

#include "rust-lex.h"
//#include "rust-tree.h"
#include "rust-scope.h"

namespace Rust {
    // Parser implementation for gccrs.
    class Parser {
      private:
        void skip_after_semicolon();
        void skip_after_end();

        bool skip_token(TokenId t);
        const_TokenPtr expect_token(TokenId t);
        void unexpected_token(const_TokenPtr t);

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

        bool done_end();
        bool done_end_or_else();
        bool done_end_of_file();

        typedef Tree (Parser::*BinaryHandler)(const_TokenPtr, Tree);
        BinaryHandler get_binary_handler(TokenId id);

      public:
        // Construct parser with specified lexer reference.
        Parser(Lexer& parLexer) : lexer(parLexer), puts_fn(), printf_fn(), scanf_fn() {}

        // Main entry point for parser.
        void parse_program();

        Tree parse_statement();

        Tree parse_variable_declaration();
        Tree parse_type_declaration();

        Tree parse_type();
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
#define BINARY_HANDLER_LIST                         \
    BINARY_HANDLER(plus, PLUS)                      \
    BINARY_HANDLER(minus, MINUS)                    \
    BINARY_HANDLER(mult, ASTERISK)                  \
    BINARY_HANDLER(div, SLASH)                      \
    BINARY_HANDLER(mod, PERCENT)                    \
                                                    \
    BINARY_HANDLER(equal, EQUAL)                    \
    BINARY_HANDLER(different, DIFFERENT)            \
    BINARY_HANDLER(smaller_than, SMALLER)           \
    BINARY_HANDLER(smaller_equal, SMALLER_OR_EQUAL) \
    BINARY_HANDLER(greater_than, GREATER)           \
    BINARY_HANDLER(greater_equal, GREATER_OR_EQUAL) \
                                                    \
    BINARY_HANDLER(logical_and, AND)                \
    BINARY_HANDLER(logical_or, OR)                  \
                                                    \
    BINARY_HANDLER(array_ref, LEFT_SQUARE)          \
                                                    \
    BINARY_HANDLER(field_ref, DOT)

// create declarations for binary op handling
#define BINARY_HANDLER(name, _) \
        Tree binary_##name(const_TokenPtr tok, Tree left);
        BINARY_HANDLER_LIST
#undef BINARY_HANDLER
    };
}

#endif // RUST_PARSE_H