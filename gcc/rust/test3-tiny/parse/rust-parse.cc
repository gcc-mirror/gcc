#include "rust-parse.h"

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "target.h"
#include "tree.h"
#include "tree-iterator.h"
#include "input.h"
#include "diagnostic.h"
#include "stringpool.h"
#include "cgraph.h"
#include "gimplify.h"
#include "gimple-expr.h"
#include "convert.h"
#include "print-tree.h"
#include "stor-layout.h"
#include "fold-const.h"
/* order: config, system, coretypes, target, tree, tree-iterator, input, diagnostic, stringpool,
 * cgraph, gimplify, gimple-expr, convert, print-tree, stor-layout, fold-const  */
// probably don't need all these

#include <algorithm> // for std::find

/* parsing notes:
 *  kinds of "syntactic units" used:
 *  - statement: expresses an action to be carried out (executed), e.g.:
 *      function calls
 *      goto
 *      return
 *      (maybe) variable definition
 *      blocks (apparently) - as in {}
 *      control structures - if, while, do-while, switch
 *     cannot return a result and are executed only for side effects
 *     distinction may not exist in rust entirely due to functional influence of lots of things being
 *      expressions
 *
 *  - expression: stuff that evaluates to a value, e.g.:
 *      2 + 3
 *      y * 6
 *
 * - variable definition (maybe - if not a statement), e.g.:
 *      y = x + 2 */

namespace Rust {
    // Left binding powers of operations.
    enum binding_powers {
        // Highest priority
        LBP_HIGHEST = 100,

        LBP_DOT = 90,

        LBP_ARRAY_REF = 80,

        LBP_UNARY_PLUS = 50,              // Used only when the null denotation is +
        LBP_UNARY_MINUS = LBP_UNARY_PLUS, // Used only when the null denotation is -

        LBP_MUL = 40,
        LBP_DIV = LBP_MUL,
        LBP_MOD = LBP_MUL,

        LBP_PLUS = 30,
        LBP_MINUS = LBP_PLUS,

        LBP_EQUAL = 20,
        LBP_DIFFERENT = LBP_EQUAL,
        LBP_SMALLER_THAN = LBP_EQUAL,
        LBP_SMALLER_EQUAL = LBP_EQUAL,
        LBP_GREATER_THAN = LBP_EQUAL,
        LBP_GREATER_EQUAL = LBP_EQUAL,

        LBP_LOGICAL_AND = 10,
        LBP_LOGICAL_OR = LBP_LOGICAL_AND,
        LBP_LOGICAL_NOT = LBP_LOGICAL_AND,

        // lowest priority
        LBP_LOWEST = 0,
    };

    // Checks if Tree has a string type (tree code pointer_type and tree variant char node).
    bool is_string_type(Tree type) {
        gcc_assert(TYPE_P(type.get_tree()));

        // ensure main variant of pointee is char_type_node (i.e. type is char*)
        return type.get_tree_code() == POINTER_TYPE
               && TYPE_MAIN_VARIANT(TREE_TYPE(type.get_tree())) == char_type_node;
    }

    // Checks if Tree has array type.
    bool is_array_type(Tree type) {
        gcc_assert(TYPE_P(type.get_tree()));
        return type.get_tree_code() == ARRAY_TYPE;
    }

    // Checks if Tree has record type.
    bool is_record_type(Tree type) {
        gcc_assert(TYPE_P(type.get_tree()));
        return type.get_tree_code() == RECORD_TYPE;
    }

    // Gets left binding power for specified token.
    int Parser::left_binding_power(const_TokenPtr token) {
        switch (token->get_id()) {
            case DOT:
                return LBP_DOT;

            case LEFT_SQUARE:
                return LBP_ARRAY_REF;

            case ASTERISK:
                return LBP_MUL;
            case SLASH:
                return LBP_DIV;
            case PERCENT:
                return LBP_MOD;

            case PLUS:
                return LBP_PLUS;
            case MINUS:
                return LBP_MINUS;

            case EQUAL:
                return LBP_EQUAL;
            case DIFFERENT:
                return LBP_DIFFERENT;
            case GREATER:
                return LBP_GREATER_THAN;
            case GREATER_OR_EQUAL:
                return LBP_GREATER_EQUAL;
            case SMALLER:
                return LBP_SMALLER_THAN;
            case SMALLER_OR_EQUAL:
                return LBP_SMALLER_EQUAL;

            case OR:
                return LBP_LOGICAL_OR;
            case AND:
                return LBP_LOGICAL_AND;
            case NOT:
                return LBP_LOGICAL_NOT;

            // anything that can't appear in an infix position is given lowest priority
            default:
                return LBP_LOWEST;
        }
    }

    TreeStmtList& Parser::get_current_stmt_list() {
        return stack_stmt_list.back();
    }

    // Parse statement sequence?
    void Parser::parse_statement_seq(bool (Parser::*done)()) {
        // Parse statements until done and append to the current stmt list
        while (!(this->*done)()) {
            // get stmt tree for parsed statement
            Tree stmt = parse_statement();
            // append each stmt tree to current stmt list
            get_current_stmt_list().append(stmt);
        }
    }

    // Returns true when current token is EOF.
    bool Parser::done_end_of_file() {
        const_TokenPtr t = lexer.peek_token();
        return (t->get_id() == END_OF_FILE);
    }

    // Entry point - parse entire program (in file) from here.
    void Parser::parse_program() {
        // Built type of main "int (int, char**)"
        tree main_fndecl_type_param[]
          = { integer_type_node, build_pointer_type(build_pointer_type(char_type_node)) };
        tree main_fndecl_type
          = build_function_type_array(integer_type_node, 2, main_fndecl_type_param);
        // Create function declaration "int main(int, char**)"
        main_fndecl = build_fn_decl("main", main_fndecl_type);

        // Enter top-level scope.
        enter_scope();
        // program -> statement*
        parse_statement_seq(&Parser::done_end_of_file);
        // Append "return 0;"
        tree resdecl = build_decl(UNKNOWN_LOCATION, RESULT_DECL, NULL_TREE, integer_type_node);
        // create decl_context of resdecl for main function (local variable of main function)
        DECL_CONTEXT(resdecl) = main_fndecl;
        DECL_RESULT(main_fndecl) = resdecl;
        tree set_result = build2(INIT_EXPR, void_type_node, DECL_RESULT(main_fndecl),
          build_int_cst_type(integer_type_node, 0));
        tree return_stmt = build1(RETURN_EXPR, void_type_node, set_result);

        get_current_stmt_list().append(return_stmt);

        // Leave top-level scope, get its binding expression and its main block
        TreeSymbolMapping main_tree_scope = leave_scope();
        Tree main_block = main_tree_scope.block;

        // Finish main function
        BLOCK_SUPERCONTEXT(main_block.get_tree()) = main_fndecl;
        DECL_INITIAL(main_fndecl) = main_block.get_tree();
        DECL_SAVED_TREE(main_fndecl) = main_tree_scope.bind_expr.get_tree();

        // Main function is not external
        DECL_EXTERNAL(main_fndecl) = 0;
        // Preserve main function
        DECL_PRESERVE_P(main_fndecl) = 1;

        // Convert from GENERIC to GIMPLE
        gimplify_function_tree(main_fndecl);

        // Insert it into the graph (queue for compilation?)
        cgraph_node::finalize_function(main_fndecl, true);

        main_fndecl = NULL_TREE;
    }

    // Parses a statement. Selects how to parse based on token id.
    Tree Parser::parse_statement() {
        /*
  statement ->  variable_declaration
          |  assignment_statement
          |  if_statement
          |  while_statement
          |  for_statement
          |  read_statement
          |  write_statement
          */
        // peek current token
        const_TokenPtr t = lexer.peek_token();

        // call method to parse statement if recognised
        switch (t->get_id()) {
            case VAR:
                return parse_variable_declaration();
                break;
            case TYPE:
                return parse_type_declaration();
                break;
            case IF:
                return parse_if_statement();
                break;
            case WHILE:
                return parse_while_statement();
                break;
            case FOR:
                return parse_for_statement();
                break;
            case READ:
                return parse_read_statement();
                break;
            case WRITE:
                return parse_write_statement();
                break;
            case IDENTIFIER:
                return parse_assignment_statement();
                break;
            default:
                // if not recognised, error with unexpected token and attempt resume
                unexpected_token(t);
                skip_after_semicolon();
                return Tree::error();
                break;
        }
    }

    // "Unexpected token" panic mode - flags gcc error at unexpected token
    void Parser::unexpected_token(const_TokenPtr t) {
        ::error_at(t->get_locus(), "unexpected %s\n", t->get_token_description());
    }

    // Crappy "error recovery" performed after error by skipping tokens until a semi-colon is found
    void Parser::skip_after_semicolon() {
        const_TokenPtr t = lexer.peek_token();

        while (t->get_id() != END_OF_FILE && t->get_id() != SEMICOLON) {
            lexer.skip_token();
            t = lexer.peek_token();
        }

        if (t->get_id() == SEMICOLON)
            lexer.skip_token();
    }

    // Parses a variable declaration statement.
    Tree Parser::parse_variable_declaration() {
        // skip initial var keyword
        if (!skip_token(VAR)) {
            skip_after_semicolon();
            return Tree::error();
        }

        // keep identifier token as used later
        const_TokenPtr identifier = expect_token(IDENTIFIER);
        if (identifier == NULL) {
            skip_after_semicolon();
            return Tree::error();
        }

        // skip colon
        if (!skip_token(COLON)) {
            skip_after_semicolon();
            return Tree::error();
        }

        // parse the actual type of the variable
        Tree type_tree = parse_type();

        if (type_tree.is_error()) {
            skip_after_semicolon();
            return Tree::error();
        }

        // skip the semicolon
        skip_token(SEMICOLON);

        // check if current mapping of the scope already contains a mapping for identifier
        if (scope.get_current_mapping().get(identifier->get_str())) {
            // create error if this is true
            error_at(identifier->get_locus(), "name %s already declared in this scope",
              identifier->get_str().c_str());
            return Tree::error();
        }

        // create a new symbol using the given identifier
        SymbolPtr sym(new Symbol(VARIABLE, identifier->get_str()));
        // put new symbol into scope mapping
        scope.get_current_mapping().insert(sym);

        // create GENERIC tree for variable declaration
        Tree decl = build_decl(identifier->get_locus(), VAR_DECL,
          get_identifier(sym->get_name().c_str()), type_tree.get_tree());
        // set decl_context of decl to main function (make it local variable of main function)
        DECL_CONTEXT(decl.get_tree()) = main_fndecl;

        // keep VAR_DECL tree in top list of stack_var_decl_chain stack for block purposes
        gcc_assert(!stack_var_decl_chain.empty());
        stack_var_decl_chain.back().append(decl);

        // associate new symbol with VAR_DECL tree
        sym->set_tree_decl(decl);

        Tree stmt = build_tree(DECL_EXPR, identifier->get_locus(), void_type_node, decl);

        return stmt;
    }

    /* Checks if current token has inputted id - skips it and returns true if so, diagnoses an error
     * and returns false otherwise. */
    bool Parser::skip_token(TokenId token_id) {
        return expect_token(token_id) != const_TokenPtr();
    }

    /* Checks the current token - if id is same as expected, skips and returns it, otherwise diagnoses
     * error and returns null. */
    const_TokenPtr Parser::expect_token(TokenId token_id) {
        const_TokenPtr t = lexer.peek_token();
        if (t->get_id() == token_id) {
            lexer.skip_token();
            return t;
        } else {
            error_at(t->get_locus(), "expecting %s but %s found!\n", get_token_description(token_id),
              t->get_token_description());

            return const_TokenPtr();
        }
    }

    // Parses type in variable declaration.
    Tree Parser::parse_type() {
        const_TokenPtr t = lexer.peek_token();

        Tree type;

        switch (t->get_id()) {
            case INT:
                lexer.skip_token();
                type = integer_type_node;
                break;
            case FLOAT:
                lexer.skip_token();
                type = float_type_node;
                break;
            case BOOL:
                lexer.skip_token();
                type = boolean_type_node;
                break;
            case IDENTIFIER: {
                SymbolPtr s = query_type(t->get_str(), t->get_locus());
                lexer.skip_token();

                if (s == NULL)
                    type = Tree::error();
                else {
                    type = TREE_TYPE(s->get_tree_decl().get_tree());
                }
            } break;
            case RECORD:
                type = parse_record();
                break;
            default:
                unexpected_token(t);
                return Tree::error();
                break;
        }

        // start parsing index ranges: list of expression pairs (lower and upper indexes of array)
        typedef std::vector<std::pair<Tree, Tree> > Dimensions;
        Dimensions dimensions;

        t = lexer.peek_token();
        while (t->get_id() == LEFT_PAREN || t->get_id() == LEFT_SQUARE) {
            lexer.skip_token();

            // array bounds
            Tree lower_bound, upper_bound;

            if (t->get_id() == LEFT_SQUARE) {
                // for array of form [e]
                Tree size = parse_integer_expression();
                skip_token(RIGHT_SQUARE);

                lower_bound = Tree(build_int_cst_type(integer_type_node, 0), size.get_locus());

                // set upper to e - 1
                upper_bound = build_tree(MINUS_EXPR, size.get_locus(), integer_type_node, size,
                  build_int_cst(integer_type_node, 1));
            } else if (t->get_id() == LEFT_PAREN) {
                // for array of form [e0:e1]
                // parse e0
                lower_bound = parse_integer_expression();
                skip_token(COLON);

                // parse e1
                upper_bound = parse_integer_expression();
                skip_token(RIGHT_PAREN);
            } else {
                gcc_unreachable();
            }

            dimensions.push_back(std::make_pair(lower_bound, upper_bound));
            t = lexer.peek_token();
        }

        // start building array type
        // transverse list in reverse order
        for (Dimensions::reverse_iterator it = dimensions.rbegin(); it != dimensions.rend(); it++) {
            // fold lower and upper expressions (simplify expressions if possible)
            it->first = Tree(fold(it->first.get_tree()), it->first.get_locus());
            it->second = Tree(fold(it->second.get_tree()), it->second.get_locus());

            if (!type.is_error()) {
                // build GCC range type using lower and upper
                Tree range_type
                  = build_range_type(integer_type_node, it->first.get_tree(), it->second.get_tree());
                // build array type
                type = build_array_type(type.get_tree(), range_type.get_tree());
            }
        }

        return type;
    }

    // Parses an if statement. Probably important to study as it seems complex.
    Tree Parser::parse_if_statement() {
        // skip if statement token
        if (!skip_token(IF)) {
            skip_after_end();
            return Tree::error();
        }

        // parse expression in statement body
        Tree expr = parse_boolean_expression();

        // skip the "THEN" after expression
        skip_token(THEN);

        // enter new block scope
        enter_scope();
        // parse statement sequence? inside if body. Finish at "end if" or "else"
        parse_statement_seq(&Parser::done_end_or_else);

        TreeSymbolMapping then_tree_scope = leave_scope();
        Tree then_stmt = then_tree_scope.bind_expr;

        Tree else_stmt;
        const_TokenPtr tok = lexer.peek_token();
        // if there is an else, parse statement seq inside its body too
        if (tok->get_id() == ELSE) {
            // Consume 'else'
            skip_token(ELSE);

            // enter block scope
            enter_scope();
            parse_statement_seq(&Parser::done_end);
            TreeSymbolMapping else_tree_scope = leave_scope();
            else_stmt = else_tree_scope.bind_expr;

            // Consume 'end'
            skip_token(END);
        } else if (tok->get_id() == END) {
            // Consume 'end'
            skip_token(END);
        } else {
            unexpected_token(tok);
            return Tree::error();
        }

        // build GENERIC if statement node.
        return build_if_statement(expr, then_stmt, else_stmt);
    }

    // Builds an if statement tree.
    Tree Parser::build_if_statement(Tree bool_expr, Tree then_part, Tree else_part) {
        if (bool_expr.is_error())
            return bool_expr;

        // create then label declaration tree
        Tree then_label_decl = build_label_decl("then", then_part.get_locus());

        // create else label declaration if it exists
        Tree else_label_decl;
        if (!else_part.is_null())
            else_label_decl = build_label_decl("else", else_part.get_locus());

        // create endif label declaration
        Tree endif_label_decl = build_label_decl("end_if", then_part.get_locus());

        // create goto expressions for entering "if" branch, "else" branch, and code after if block
        Tree goto_then
          = build_tree(GOTO_EXPR, bool_expr.get_locus(), void_type_node, then_label_decl);
        Tree goto_endif
          = build_tree(GOTO_EXPR, bool_expr.get_locus(), void_type_node, endif_label_decl);

        Tree goto_else_or_endif;
        if (!else_part.is_null())
            goto_else_or_endif
              = build_tree(GOTO_EXPR, bool_expr.get_locus(), void_type_node, else_label_decl);
        else
            goto_else_or_endif = goto_endif;

        // create statement list for if statement which will have required statements appended
        TreeStmtList stmt_list;

        // create conditional branch expression and append to stmt_list
        Tree cond_expr = build_tree(
          COND_EXPR, bool_expr.get_locus(), void_type_node, bool_expr, goto_then, goto_else_or_endif);
        stmt_list.append(cond_expr);

        // define location related to "then" part and append to stmt_list
        Tree then_label_expr
          = build_tree(LABEL_EXPR, then_part.get_locus(), void_type_node, then_label_decl);
        stmt_list.append(then_label_expr);

        // append parameter "then_part" to statement list
        stmt_list.append(then_part);

        // if else part exists, append a goto endif
        if (!else_part.is_null()) {
            // Make sure after then part has been executed we go to the end if
            stmt_list.append(goto_endif);

            // define location of else label, append it, and append else_part parameter tree
            Tree else_label_expr
              = build_tree(LABEL_EXPR, else_part.get_locus(), void_type_node, else_label_decl);
            stmt_list.append(else_label_expr);
            stmt_list.append(else_part);
            // do not need to jump to endif as handled implicitly here
        }

        // define label for endif, append to statement list
        Tree endif_label_expr
          = build_tree(LABEL_EXPR, UNKNOWN_LOCATION, void_type_node, endif_label_decl);
        stmt_list.append(endif_label_expr);

        // return the statement list in tree form
        return stmt_list.get_tree();
    }

    // Builds a GENERIC tree LABEL_DECL (represents a label, as in a "goto" label).
    Tree Parser::build_label_decl(const char* name, location_t loc) {
        tree t = build_decl(loc, LABEL_DECL, get_identifier(name), void_type_node);

        gcc_assert(main_fndecl != NULL_TREE);
        DECL_CONTEXT(t) = main_fndecl;

        return t;
    }

    // Skips all tokens until EOF.
    void Parser::skip_after_end() {
        const_TokenPtr t = lexer.peek_token();

        while (t->get_id() != END_OF_FILE && t->get_id() != END) {
            lexer.skip_token();
            t = lexer.peek_token();
        }

        if (t->get_id() == END) {
            lexer.skip_token();
        }
    }

    // Pratt parser impl of parse_expression.
    Tree Parser::parse_expression(int right_binding_power) {
        const_TokenPtr current_token = lexer.peek_token();
        lexer.skip_token();

        Tree expr = null_denotation(current_token);

        if (expr.is_error()) {
            return Tree::error();
        }

        // stop parsing if find lower priority token - parse higher priority first
        while (right_binding_power < left_binding_power(lexer.peek_token())) {
            current_token = lexer.peek_token();
            lexer.skip_token();

            expr = left_denotation(current_token, expr);
            if (expr.is_error())
                return Tree::error();
        }

        return expr;
    }

    // Parse an expression with lowest left binding power.
    Tree Parser::parse_expression() {
        return parse_expression(LBP_LOWEST);
    }

    // Parses a boolean expression (basically parses expression and ensures boolean result).
    Tree Parser::parse_boolean_expression() {
        Tree expr = parse_expression();
        if (expr.is_error())
            return expr;

        if (expr.get_type() != boolean_type_node) {
            error_at(expr.get_locus(), "expected expression of boolean type but its type is %s",
              print_type(expr.get_type()));
            return Tree::error();
        }

        return expr;
    }

    // Parses an integer expression (basically parses expression and ensures integer result).
    Tree Parser::parse_integer_expression() {
        Tree expr = parse_expression();
        if (expr.is_error())
            return expr;

        if (expr.get_type() != integer_type_node) {
            error_at(expr.get_locus(), "expected expression of integer type but its type is %s",
              print_type(expr.get_type()));
            return Tree::error();
        }

        return expr;
    }

    // Determines action to take when finding token at beginning of expression.
    Tree Parser::null_denotation(const_TokenPtr tok) {
        // note: tok is previous character in input stream, not current one, as parse_expression
        // skips it before passing it in

        /* as a Pratt parser (which works by decomposing expressions into a null denotation and then a
         * left denotation), null denotations handle primaries and unary operands */

        switch (tok->get_id()) {
            case IDENTIFIER: {
                // when encountering identifier, lookup in scope
                SymbolPtr s = scope.lookup(tok->get_str());
                if (s == NULL) {
                    error_at(tok->get_locus(), "variable '%s' not declared in the current scope",
                      tok->get_str().c_str());

                    return Tree::error();
                }
                // expression is just its VAR_DECL that was stored in the Symbol at declaration
                return Tree(s->get_tree_decl(), tok->get_locus());
            }
            case INT_LITERAL:
                // we should check the range, but ignore for now
                // literal itself encodes value, so token's text has to be interpreted as int. use
                // atoi for this
                return Tree(build_int_cst_type(integer_type_node, atoi(tok->get_str().c_str())),
                  tok->get_locus());
            case FLOAT_LITERAL: {
                REAL_VALUE_TYPE float_value;
                // invoke real_from_string3 to get float value representation from string
                real_from_string3(&float_value, tok->get_str().c_str(), TYPE_MODE(float_type_node));
                // this is because machine-dependent

                // create actual tree with that built constant value
                return Tree(build_real(float_type_node, float_value), tok->get_locus());
            }
            case STRING_LITERAL: {
                // get c string from token
                std::string str = tok->get_str();
                const char* c_str = str.c_str();

                // use GCC's build_string_literal (with null terminator) to create tree
                return Tree(build_string_literal(strlen(c_str) + 1, c_str), tok->get_locus());
            }
            case TRUE_LITERAL: {
                // construct tree with code INTEGER_CST and value 1 but with boolean_type_node
                return Tree(build_int_cst_type(boolean_type_node, 1), tok->get_locus());
                break;
            }
            case FALSE_LITERAL: {
                return Tree(build_int_cst_type(boolean_type_node, 0), tok->get_locus());
                break;
            }
            case LEFT_PAREN: { // have to parse whole expression if inside brackets
                /* recursively invoke parse_expression with lowest priority possible as it it were
                 * a top-level expression. */
                Tree expr = parse_expression();
                tok = lexer.peek_token();

                // end of expression must be a close-bracket
                if (tok->get_id() != RIGHT_PAREN)
                    error_at(
                      tok->get_locus(), "expecting ')' but %s found\n", tok->get_token_description());
                else
                    lexer.skip_token();

                return Tree(expr, tok->get_locus());
            }
            case PLUS: { // unary plus operator
                // invoke parse_expression recursively with appropriate priority, etc. for below
                Tree expr = parse_expression(LBP_UNARY_PLUS);

                if (expr.is_error())
                    return Tree::error();
                // can only apply to integer and float expressions
                if (expr.get_type() != integer_type_node || expr.get_type() != float_type_node) {
                    error_at(tok->get_locus(),
                      "operand of unary plus must be int or float but it is %s",
                      print_type(expr.get_type()));
                    return Tree::error();
                }

                return Tree(expr, tok->get_locus());
            }
            case MINUS: { // unary minus
                Tree expr = parse_expression(LBP_UNARY_MINUS);

                if (expr.is_error())
                    return Tree::error();
                // can only apply to integer and float expressions
                if (expr.get_type() != integer_type_node || expr.get_type() != float_type_node) {
                    error_at(tok->get_locus(),
                      "operand of unary minus must be int or float but it is %s",
                      print_type(expr.get_type()));
                    return Tree::error();
                }

                // create NEGATE_EXPR tree, which computes negation of operand
                expr = build_tree(NEGATE_EXPR, tok->get_locus(), expr.get_type(), expr);
                return expr;
            }
            case NOT: { // logical not
                Tree expr = parse_expression(LBP_LOGICAL_NOT);

                if (expr.is_error())
                    return Tree::error();
                // can only apply to boolean expressions
                if (expr.get_type() != boolean_type_node) {
                    error_at(tok->get_locus(),
                      "operand of logical not must be a boolean but it is %s",
                      print_type(expr.get_type()));
                    return Tree::error();
                }

                // create TRUTH_NOT_EXPR tree, which computes logical negation of operand
                expr = build_tree(TRUTH_NOT_EXPR, tok->get_locus(), boolean_type_node, expr);
                return expr;
            }
            default:
                unexpected_token(tok);
                return Tree::error();
        }
    }

    /* Called for each token that can appear in infix (between) position. Can be operators or other
     * punctuation.
     * Returns a function pointer to member function that implements the left denotation for the token
     * given. */
    Tree Parser::left_denotation(const_TokenPtr tok, Tree left) {
        BinaryHandler binary_handler = get_binary_handler(tok->get_id());
        if (binary_handler == NULL) {
            unexpected_token(tok);
            return Tree::error();
        }

        return (this->*binary_handler)(tok, left);
    }

    // Gets method for handling binary operation parsing for specific token type.
    Parser::BinaryHandler Parser::get_binary_handler(TokenId id) {
        switch (id) {
#define BINARY_HANDLER(name, token_id) \
    case token_id:                     \
        return &Parser::binary_##name;
            BINARY_HANDLER_LIST
#undef BINARY_HANDLER
            default:
                return NULL;
        }
    }

    /* Returns the type of the binary operation. May also modify input trees if types do not match,
     * e.g. change a float and int to two floats in addition.   */
    Tree Parser::coerce_binary_arithmetic(const_TokenPtr tok, Tree* left, Tree* right) {
        Tree left_type = left->get_type();
        Tree right_type = right->get_type();

        // return error if either types are invalid
        if (left_type.is_error() || right_type.is_error())
            return Tree::error();

        // good, easy, no type conversion required
        if (left_type == right_type) {
            if (left_type == integer_type_node || left_type == float_type_node) {
                return left_type;
            }
            // dodgy type coercion happening if types don't match but are both numerical
        } else if ((left_type == integer_type_node && right_type == float_type_node)
                   || (left_type == float_type_node && right_type == integer_type_node)) {
            // rebuild one tree as float
            if (left_type == integer_type_node) {
                *left = build_tree(FLOAT_EXPR, left->get_locus(), float_type_node, left->get_tree());
            } else {
                *right
                  = build_tree(FLOAT_EXPR, right->get_locus(), float_type_node, right->get_tree());
            }

            return float_type_node;
        }

        // unhandled - e.g. int + boolean
        error_at(tok->get_locus(), "invalid operands of type %s and %s for operator %s",
          print_type(left_type), print_type(right_type), tok->get_token_description());
        return Tree::error();
    }

    // Verifies that both left and right trees are boolean-type nodes.
    bool Parser::check_logical_operands(const_TokenPtr tok, Tree left, Tree right) {
        // ensure both operands are boolean types
        if (left.get_type() != boolean_type_node || right.get_type() != boolean_type_node) {
            error_at(tok->get_locus(),
              "operands of operator %s must be boolean but they are %s and %s\n",
              tok->get_token_description(), print_type(left.get_type()),
              print_type(right.get_type()));

            return false;
        }

        return true;
    }

    // Implementation of addition expression parsing.
    Tree Parser::binary_plus(const_TokenPtr tok, Tree left) {
        // parse RHS (as tok has already been consumed in parse_expression)
        Tree right = parse_expression(LBP_PLUS);
        if (right.is_error())
            return Tree::error();

        /* compute resulting type of binary operator with coerce_binary_arithmetic method, which may
         * also modify input trees.    */
        Tree tree_type = coerce_binary_arithmetic(tok, &left, &right);
        if (tree_type.is_error())
            return Tree::error();

        // construct tree with code PLUS_EXPR to represent binary addition
        return build_tree(PLUS_EXPR, tok->get_locus(), tree_type, left, right);
    }

    // Implementation of subtraction expression parsing.
    Tree Parser::binary_minus(const_TokenPtr tok, Tree left) {
        // parse RHS (as tok has already been consumed in parse_expression)
        Tree right = parse_expression(LBP_PLUS);
        if (right.is_error())
            return Tree::error();

        /* compute resulting type of binary operator with coerce_binary_arithmetic method, which may
         * also modify input trees.    */
        Tree tree_type = coerce_binary_arithmetic(tok, &left, &right);
        if (tree_type.is_error())
            return Tree::error();

        // construct tree with code MINUS_EXPR to represent binary subtraction
        return build_tree(MINUS_EXPR, tok->get_locus(), tree_type, left, right);
    }

    // Implementation of multiplication expression parsing.
    Tree Parser::binary_mult(const_TokenPtr tok, Tree left) {
        // parse RHS (as tok has already been consumed in parse_expression)
        Tree right = parse_expression(LBP_PLUS);
        if (right.is_error())
            return Tree::error();

        /* compute resulting type of binary operator with coerce_binary_arithmetic method, which may
         * also modify input trees.    */
        Tree tree_type = coerce_binary_arithmetic(tok, &left, &right);
        if (tree_type.is_error())
            return Tree::error();

        // construct tree with code MULT_EXPR to represent binary multiplication
        return build_tree(MULT_EXPR, tok->get_locus(), tree_type, left, right);
    }

    // Implementation of divison expression parsing.
    Tree Parser::binary_div(const_TokenPtr tok, Tree left) {
        // parse RHS
        Tree right = parse_expression(LBP_DIV);
        if (right.is_error())
            return Tree::error();

        // if integer division, create C-like truncated division expression tree
        if (left.get_type() == integer_type_node && right.get_type() == integer_type_node) {
            return build_tree(TRUNC_DIV_EXPR, tok->get_locus(), integer_type_node, left, right);
        } else {
            // floating-point division
            Tree tree_type = coerce_binary_arithmetic(tok, &left, &right);
            if (tree_type.is_error())
                return Tree::error();

            gcc_assert(tree_type == float_type_node);

            return build_tree(RDIV_EXPR, tok->get_locus(), tree_type, left, right);
        }
    }

    // Implementation of modulo expression parsing.
    Tree Parser::binary_mod(const_TokenPtr tok, Tree left) {
        // parse RHS
        Tree right = parse_expression(LBP_MOD);
        if (right.is_error())
            return Tree::error();

        // if integer modulo, create truncated modulo expression
        if (left.get_type() == integer_type_node && right.get_type() == integer_type_node) {
            return build_tree(TRUNC_MOD_EXPR, tok->get_locus(), integer_type_node, left, right);
        } else {
            // no floating-point modulo
            return Tree::error();
        }
    }

    // Implementation of binary equal comparison relational operator parsing.
    Tree Parser::binary_equal(const_TokenPtr tok, Tree left) {
        // parse RHS
        Tree right = parse_expression(LBP_EQUAL);
        if (right.is_error())
            return Tree::error();

        Tree tree_type = coerce_binary_arithmetic(tok, &left, &right);
        if (tree_type.is_error())
            return Tree::error();

        return build_tree(EQ_EXPR, tok->get_locus(), boolean_type_node, left, right);
    }

    // Implementation of binary different comparison relational operator parsing.
    Tree Parser::binary_different(const_TokenPtr tok, Tree left) {
        // parse RHS
        Tree right = parse_expression(LBP_DIFFERENT);
        if (right.is_error())
            return Tree::error();

        Tree tree_type = coerce_binary_arithmetic(tok, &left, &right);
        if (tree_type.is_error())
            return Tree::error();

        return build_tree(NE_EXPR, tok->get_locus(), boolean_type_node, left, right);
    }

    // Implementation of binary smaller than comparison relational operator parsing.
    Tree Parser::binary_smaller_than(const_TokenPtr tok, Tree left) {
        // parse RHS
        Tree right = parse_expression(LBP_SMALLER_THAN);
        if (right.is_error())
            return Tree::error();

        Tree tree_type = coerce_binary_arithmetic(tok, &left, &right);
        if (tree_type.is_error())
            return Tree::error();

        return build_tree(LT_EXPR, tok->get_locus(), boolean_type_node, left, right);
    }

    // Implementation of binary greater than comparison relational operator parsing.
    Tree Parser::binary_greater_than(const_TokenPtr tok, Tree left) {
        // parse RHS
        Tree right = parse_expression(LBP_GREATER_THAN);
        if (right.is_error())
            return Tree::error();

        Tree tree_type = coerce_binary_arithmetic(tok, &left, &right);
        if (tree_type.is_error())
            return Tree::error();

        return build_tree(GT_EXPR, tok->get_locus(), boolean_type_node, left, right);
    }

    // Implementation of binary smaller than or equal to comparison relational operator parsing.
    Tree Parser::binary_smaller_equal(const_TokenPtr tok, Tree left) {
        // parse RHS
        Tree right = parse_expression(LBP_SMALLER_EQUAL);
        if (right.is_error())
            return Tree::error();

        Tree tree_type = coerce_binary_arithmetic(tok, &left, &right);
        if (tree_type.is_error())
            return Tree::error();

        return build_tree(LE_EXPR, tok->get_locus(), boolean_type_node, left, right);
    }

    // Implementation of binary greater than or equal to comparison relational operator parsing.
    Tree Parser::binary_greater_equal(const_TokenPtr tok, Tree left) {
        // parse RHS
        Tree right = parse_expression(LBP_GREATER_EQUAL);
        if (right.is_error())
            return Tree::error();

        Tree tree_type = coerce_binary_arithmetic(tok, &left, &right);
        if (tree_type.is_error())
            return Tree::error();

        return build_tree(GE_EXPR, tok->get_locus(), boolean_type_node, left, right);
    }

    // Implementation of binary "and" logical operator parsing.
    Tree Parser::binary_logical_and(const_TokenPtr tok, Tree left) {
        // parse RHS
        Tree right = parse_expression(LBP_LOGICAL_AND);
        if (right.is_error())
            return Tree::error();

        if (!check_logical_operands(tok, left, right))
            return Tree::error();

        return build_tree(TRUTH_ANDIF_EXPR, tok->get_locus(), boolean_type_node, left, right);
    }

    // Implementation of binary "or" logical operator parsing.
    Tree Parser::binary_logical_or(const_TokenPtr tok, Tree left) {
        // parse RHS
        Tree right = parse_expression(LBP_LOGICAL_OR);
        if (right.is_error())
            return Tree::error();

        if (!check_logical_operands(tok, left, right))
            return Tree::error();

        return build_tree(TRUTH_ORIF_EXPR, tok->get_locus(), boolean_type_node, left, right);
    }

    // Implementation of binary array reference ([) operator parsing;
    Tree Parser::binary_array_ref(const_TokenPtr tok, Tree left) {
        // parse integer expression inside square brackets (array index)
        Tree right = parse_integer_expression();
        if (right.is_error())
            return Tree::error();

        // array close token
        if (!skip_token(RIGHT_SQUARE))
            return Tree::error();

        // verify left operand has array type
        if (!is_array_type(left.get_type())) {
            error_at(left.get_locus(), "does not have array type");
            return Tree::error();
        }

        // compute type of array element
        Tree element_type = TREE_TYPE(left.get_type().get_tree());

        // build GENERIC tree ARRAY_REF that represents array access
        return build_tree(ARRAY_REF, tok->get_locus(), element_type, left, right, Tree(), Tree());
    }

    // Parses a binary field access on a record.
    Tree Parser::binary_field_ref(const_TokenPtr tok, Tree left) {
        const_TokenPtr identifier = expect_token(IDENTIFIER);
        if (identifier == NULL)
            return Tree::error();

        // ensure left expression has record type
        if (!is_record_type(left.get_type())) {
            error_at(left.get_locus(), "does not have record type");
            return Tree::error();
        }

        // traverse each FIELD_DECL chaining through TREE_CHAIN
        // list of fields in record type is available through TYPE_FIELDS
        Tree field_decl = TYPE_FIELDS(left.get_type().get_tree());
        while (!field_decl.is_null()) {
            // FIELD_DECL has a DECL_NAME containing an IDENTIFIER_POINTER to get field name
            Tree decl_name = DECL_NAME(field_decl.get_tree());
            const char* field_name = IDENTIFIER_POINTER(decl_name.get_tree());

            if (field_name == identifier->get_str())
                break;

            field_decl = TREE_CHAIN(field_decl.get_tree());
        }

        // if can't find a field with given name, this is an error
        if (field_decl.is_null()) {
            error_at(left.get_locus(), "record type does not have a field named '%s'",
              identifier->get_str().c_str());
            return Tree::error();
        }

        // build COMPONENT_REF tree using left tree (record type) and appropriate FIELD_DECL
        return build_tree(COMPONENT_REF, tok->get_locus(), TREE_TYPE(field_decl.get_tree()), left,
          field_decl, Tree());
    }

    // Parse variable assignment statement. This is not the same as variable declaration.
    Tree Parser::parse_assignment_statement() {
        Tree variable = parse_lhs_assignment_expression();

        if (variable.is_error())
            return Tree::error();

        const_TokenPtr assig_tok = expect_token(ASSIG);
        if (assig_tok == NULL) {
            skip_after_semicolon();
            return Tree::error();
        }

        // skip assignment token and parse expression
        const_TokenPtr first_of_expr = lexer.peek_token();

        Tree expr = parse_expression();
        if (expr.is_error())
            return Tree::error();

        // skip semicolon token
        skip_token(SEMICOLON);

        // enforce rule that rhs of assignment has to have same type as declared lhs type
        if (variable.get_type() != expr.get_type()) {
            // diagnostic
            error_at(first_of_expr->get_locus(),
              "cannot assign value of type %s to a variable of type %s", print_type(expr.get_type()),
              print_type(variable.get_type()));

            return Tree::error();
        }

        Tree assig_expr
          = build_tree(MODIFY_EXPR, assig_tok->get_locus(), void_type_node, variable, expr);
        return assig_expr;
    }

    // Print human-readable name for type.
    const char* Parser::print_type(Tree type) {
        gcc_assert(TYPE_P(type.get_tree()));

        if (type == void_type_node)
            return "void";
        else if (type == integer_type_node)
            return "int";
        else if (type == float_type_node)
            return "float";
        else if (is_string_type(type))
            return "string";
        else if (type == boolean_type_node)
            return "boolean";
        else
            return "<<unknown-type>>";
    }

    // Returns address to function declaration of printf.
    Tree Parser::get_printf_addr() {
        // only run if printf_fn is null to avoid making repeated function declarations
        if (printf_fn.is_null()) {
            // build const char* type (printf fixed parameter)
            tree fndecl_type_param[]
              = { build_pointer_type(build_qualified_type(char_type_node, TYPE_QUAL_CONST)) };

            // build function type as vararg function
            tree fndecl_type
              = build_varargs_function_type_array(integer_type_node, 1, fndecl_type_param);

            // build declaration
            tree printf_fn_decl = build_fn_decl("printf", fndecl_type);
            // mark as external
            DECL_EXTERNAL(printf_fn_decl) = 1;

            // build an ADDR_EXPR, which returns a pointer to type of function type (function address)
            printf_fn = build1(ADDR_EXPR, build_pointer_type(fndecl_type), printf_fn_decl);
        }

        return printf_fn;
    }

    // Returns address to function declaration of puts.
    Tree Parser::get_puts_addr() {
        if (puts_fn.is_null()) {
            // build const char* type (puts fixed parameter)
            tree fndecl_type_param[]
              = { build_pointer_type(build_qualified_type(char_type_node, TYPE_QUAL_CONST)) };

            // build function type
            tree fndecl_type = build_function_type_array(integer_type_node, 1, fndecl_type_param);

            // build declaration
            tree puts_fn_decl = build_fn_decl("puts", fndecl_type);
            // mark as external
            DECL_EXTERNAL(puts_fn_decl) = 1;

            // build an ADDR_EXPR, which returns a pointer to type of function type (function address)
            puts_fn = build1(ADDR_EXPR, build_pointer_type(fndecl_type), puts_fn_decl);
        }

        return puts_fn;
    }

    // Returns address to function declaration of scanf.
    Tree Parser::get_scanf_addr() {
        // only run if scanf_fn is null to avoid making repeated function declarations
        if (scanf_fn.is_null()) {
            // build const char* type (scanf fixed parameter)
            tree fndecl_type_param[]
              = { build_pointer_type(build_qualified_type(char_type_node, TYPE_QUAL_CONST)) };

            // build function type as vararg function
            tree fndecl_type
              = build_varargs_function_type_array(integer_type_node, 1, fndecl_type_param);

            // build declaration
            tree scanf_fn_decl = build_fn_decl("scanf", fndecl_type);
            // mark as external
            DECL_EXTERNAL(scanf_fn_decl) = 1;

            // build an ADDR_EXPR, which returns a pointer to type of function type (function address)
            scanf_fn = build1(ADDR_EXPR, build_pointer_type(fndecl_type), scanf_fn_decl);
        }

        return scanf_fn;
    }

    // Parses a "write statement".
    Tree Parser::parse_write_statement() {
        // write_statement -> "write" expression ";"

        if (!skip_token(WRITE)) {
            skip_after_semicolon();
            return Tree::error();
        }

        const_TokenPtr first_of_expr = lexer.peek_token();
        Tree expr = parse_expression();

        skip_token(SEMICOLON);

        if (expr.is_error())
            return Tree::error();

        // enable printing of value of expression
        if (expr.get_type() == integer_type_node) {
            // printf("%d\n", expr)
            // build format string for integer (also add null terminator) and integer expression
            const char* format_integer = "%d\n";
            tree args[]
              = { build_string_literal(strlen(format_integer) + 1, format_integer), expr.get_tree() };

            // used as trees do not allow calling a FUNCTIONAL_DECL directly
            Tree printf_fn = get_printf_addr();

            // build call to print function (printf_fn), in which two arguments in args are passed
            // first argument is format string
            tree stmt = build_call_array_loc(
              first_of_expr->get_locus(), integer_type_node, printf_fn.get_tree(), 2, args);

            return stmt;
        } else if (expr.get_type() == float_type_node) {
            // printf("%f\n" (double)expr)
            // have to convert float to double
            const char* format_float = "%f\n";
            tree args[] = { build_string_literal(strlen(format_float) + 1, format_float),
                convert(double_type_node, expr.get_tree()) };

            Tree printf_fn = get_printf_addr();

            // build call, etc.
            tree stmt = build_call_array_loc(
              first_of_expr->get_locus(), integer_type_node, printf_fn.get_tree(), 2, args);

            return stmt;
        } else if (is_string_type(expr.get_type())) {
            // Alternatively we could use printf('%s\n', expr) instead of puts(expr)
            tree args[] = { expr.get_tree() };

            Tree puts_fn = get_puts_addr();

            tree stmt = build_call_array_loc(
              first_of_expr->get_locus(), integer_type_node, puts_fn.get_tree(), 1, args);

            return stmt;
        } else {
            // no more valid types
            error_at(first_of_expr->get_locus(), "value of type %s is not a valid write operand",
              print_type(expr.get_type()));
            return Tree::error();
        }

        gcc_unreachable();
    }

    // Enters new scope (like block scope or whatever).
    void Parser::enter_scope() {
        // push new symbol mapping
        scope.push_scope();

        TreeStmtList stmt_list;
        // Used as stack for statements.
        stack_stmt_list.push_back(stmt_list);

        // Used as stack of var decls.
        stack_var_decl_chain.push_back(TreeChain());
        // Used as stack for blocks.
        stack_block_chain.push_back(BlockChain());
    }

    // Leaves current scope (as defined by blocks - like block scope).
    Parser::TreeSymbolMapping Parser::leave_scope() {
        // Get current list of statements and pop them from stack of statement lists
        TreeStmtList current_stmt_list = get_current_stmt_list();
        stack_stmt_list.pop_back();

        // Get current list of var decls and pop them from stack of var decl lists
        TreeChain var_decl_chain = stack_var_decl_chain.back();
        stack_var_decl_chain.pop_back();

        // Get current list of blocks and pop them from stack of block lists
        BlockChain subblocks = stack_block_chain.back();
        stack_block_chain.pop_back();

        // Create a new block from var decls and subblocks
        tree new_block = build_block(var_decl_chain.first.get_tree(), subblocks.first.get_tree(),
          /* supercontext */ NULL_TREE, /* chain */ NULL_TREE);

        // Add the new block to the current chain of blocks (if any)
        if (!stack_block_chain.empty()) {
            stack_block_chain.back().append(new_block);
        }

        // Set the subblocks to have the new block as their parent
        for (tree it = subblocks.first.get_tree(); it != NULL_TREE; it = BLOCK_CHAIN(it))
            BLOCK_SUPERCONTEXT(it) = new_block;
        // Do it this way because of double-linkage

        // Create BIND_EXPR from decl chain, stmt list, and new block
        tree bind_expr = build3(BIND_EXPR, void_type_node, var_decl_chain.first.get_tree(),
          current_stmt_list.get_tree(), new_block);

        // create, basically, a tuple of bind_expr and new_block
        TreeSymbolMapping tree_scope;
        tree_scope.bind_expr = bind_expr;
        tree_scope.block = new_block;

        // pop symbol mapping
        scope.pop_scope();

        return tree_scope;
    }

    // Parses the "read" statement.
    Tree Parser::parse_read_statement() {
        if (!skip_token(READ)) {
            skip_after_semicolon();
            return Tree::error();
        }

        const_TokenPtr first_of_expr = lexer.peek_token();
        Tree expr = parse_expression_naming_variable();

        skip_token(SEMICOLON);

        if (expr.is_error())
            return Tree::error();

        // force variable name instead of manually looking up identifier token
        /* if (expr.get_tree_code() != VAR_DECL) {
            error_at(first_of_expr->get_locus(), "invalid expression in read statement");
            return Tree::error();
        }*/
        // not used anymore due to parse_expression_naming_variable

        // Variable must be addressable (variable needs address computed)
        TREE_ADDRESSABLE(expr.get_tree()) = 1;

        // determine appropriate format string
        const char* format = NULL;
        if (expr.get_type() == integer_type_node) {
            format = "%d";
        } else if (expr.get_type() == float_type_node) {
            format = "%f";
        } else {
            error_at(first_of_expr->get_locus(), "variable of type %s is not a valid read operand",
              print_type(expr.get_type()));
            return Tree::error();
        }

        // build args for scanf
        tree args[] = { build_string_literal(strlen(format) + 1, format),
            build_tree(ADDR_EXPR, first_of_expr->get_locus(),
              build_pointer_type(expr.get_type().get_tree()), expr)
              .get_tree() };

        // get scanf address
        Tree scanf_fn = get_scanf_addr();

        // create tree to call scanf
        tree stmt = build_call_array_loc(
          first_of_expr->get_locus(), integer_type_node, scanf_fn.get_tree(), 2, args);

        return stmt;
    }

    // Parses a while statement.
    Tree Parser::parse_while_statement() {
        if (!skip_token(WHILE)) {
            skip_after_end();
            return Tree::error();
        }

        // parse while's conditional expression
        Tree expr = parse_boolean_expression();
        if (!skip_token(DO)) {
            skip_after_end();
            return Tree::error();
        }

        // enter loop body scope
        enter_scope();
        parse_statement_seq(&Parser::done_end);
        TreeSymbolMapping while_body_tree_scope = leave_scope();

        Tree while_body_stmt = while_body_tree_scope.bind_expr;

        skip_token(END);

        // build while statement tree
        return build_while_statement(expr, while_body_stmt);
    }

    // Builds a while statement tree.
    Tree Parser::build_while_statement(Tree bool_expr, Tree while_body) {
        if (bool_expr.is_error())
            return Tree::error();

        TreeStmtList stmt_list;

        // build label decl for while condition check
        Tree while_check_label_decl = build_label_decl("while_check", bool_expr.get_locus());

        // build label expr for while condition check and add to statement list
        Tree while_check_label_expr
          = build_tree(LABEL_EXPR, bool_expr.get_locus(), void_type_node, while_check_label_decl);
        stmt_list.append(while_check_label_expr);

        // build label decl for loop body and end of loop
        Tree while_body_label_decl = build_label_decl("while_body", while_body.get_locus());
        Tree end_of_while_label_decl = build_label_decl("end_of_while", UNKNOWN_LOCATION);

        // add cond_expr tree that evaluates condition expression and branches to correct label
        Tree cond_expr = build_tree(COND_EXPR, bool_expr.get_locus(), void_type_node, bool_expr,
          build_tree(GOTO_EXPR, bool_expr.get_locus(), void_type_node, while_body_label_decl),
          build_tree(GOTO_EXPR, bool_expr.get_locus(), void_type_node, end_of_while_label_decl));
        stmt_list.append(cond_expr);

        // define location of label for body of loop and append to while body
        Tree while_body_label_expr
          = build_tree(LABEL_EXPR, while_body.get_locus(), void_type_node, while_body_label_decl);
        stmt_list.append(while_body_label_expr);

        stmt_list.append(while_body);

        // branch back to condition check (as it is a loop)
        Tree goto_check
          = build_tree(GOTO_EXPR, UNKNOWN_LOCATION, void_type_node, while_check_label_decl);
        stmt_list.append(goto_check);

        // define location of label for end of the while loop
        Tree end_of_while_label_expr
          = build_tree(LABEL_EXPR, UNKNOWN_LOCATION, void_type_node, end_of_while_label_decl);
        stmt_list.append(end_of_while_label_expr);

        return stmt_list.get_tree();
    }

    // Parse a for statement.
    Tree Parser::parse_for_statement() {
        // for -> for <identifier> := <expression> to <expression> do <statements> end
        if (!skip_token(FOR)) {
            skip_after_end();
            return Tree::error();
        }

        const_TokenPtr identifier = expect_token(IDENTIFIER);
        if (identifier == NULL) {
            skip_after_end();
            return Tree::error();
        }

        if (!skip_token(ASSIG)) {
            skip_after_end();
            return Tree::error();
        }

        // parse lower bound expression
        Tree lower_bound = parse_integer_expression();

        if (!skip_token(TO)) {
            skip_after_end();
            return Tree::error();
        }

        // parse upper bound expression
        Tree upper_bound = parse_integer_expression();

        if (!skip_token(DO)) {
            skip_after_end();
            return Tree::error();
        }

        // enter loop body scope and parse internal statements
        enter_scope();
        parse_statement_seq(&Parser::done_end);

        TreeSymbolMapping for_body_tree_scope = leave_scope();
        Tree for_body_stmt = for_body_tree_scope.bind_expr;

        skip_token(END);

        // Induction variable ("loop counter" variable) handling
        SymbolPtr ind_var = query_integer_variable(identifier->get_str(), identifier->get_locus());

        // build for statement
        return build_for_statement(ind_var, lower_bound, upper_bound, for_body_stmt);
    }

    // Builds a for statement tree (piggybacks on while statement tree building).
    Tree Parser::build_for_statement(
      SymbolPtr ind_var, Tree lower_bound, Tree upper_bound, Tree for_body_stmt_list) {
        if (ind_var == NULL)
            return Tree::error();
        Tree ind_var_decl = ind_var->get_tree_decl();

        // lower
        if (lower_bound.is_error())
            return Tree::error();

        // upper
        if (upper_bound.is_error())
            return Tree::error();

        // ind_var = lower
        TreeStmtList stmt_list;

        // initialise induction variable with value of lower bound and append to stmt_list
        Tree init_ind_var
          = build_tree(MODIFY_EXPR, UNKNOWN_LOCATION, void_type_node, ind_var_decl, lower_bound);
        stmt_list.append(init_ind_var);

        // define condition ind_var <= upper for use in while loop
        Tree while_condition = build_tree(
          LE_EXPR, upper_bound.get_locus(), boolean_type_node, ind_var_decl, upper_bound);

        // simulate incrementing ind_var
        Tree incr_ind_var = build_tree(MODIFY_EXPR, UNKNOWN_LOCATION, void_type_node, ind_var_decl,
          build_tree(PLUS_EXPR, UNKNOWN_LOCATION, integer_type_node, ind_var_decl,
            build_int_cst_type(integer_type_node, 1)));

        // Wrap as stmt list
        TreeStmtList for_stmt_list = for_body_stmt_list;
        for_stmt_list.append(incr_ind_var);

        // construct associated while statement and append to stmt_list
        Tree while_stmt = build_while_statement(while_condition, for_stmt_list.get_tree());
        stmt_list.append(while_stmt);

        // return entire stmt_list
        return stmt_list.get_tree();
    }

    // Gets type (as in typedef) of name in current scope.
    SymbolPtr Parser::query_type(const std::string& name, location_t loc) {
        SymbolPtr sym = scope.lookup(name);
        if (sym == NULL) {
            error_at(loc, "type '%s' not declared in the current scope", name.c_str());
        } else if (sym->get_kind() != TYPENAME) {
            error_at(loc, "name '%s' is not a type", name.c_str());
            sym = SymbolPtr();
        }

        return sym;
    }

    // Get variable of name in current scope.
    SymbolPtr Parser::query_variable(const std::string& name, location_t loc) {
        SymbolPtr sym = scope.lookup(name);
        if (sym == NULL) {
            error_at(loc, "variable '%s' not declared in the current scope", name.c_str());
        } else if (sym->get_kind() != VARIABLE) {
            error_at(loc, "name '%s' is not a variable", name.c_str());
            sym = SymbolPtr();
        }
        return sym;
    }

    // Gets variable of name in current scope and ensures it has integer type.
    SymbolPtr Parser::query_integer_variable(const std::string& name, location_t loc) {
        SymbolPtr sym = query_variable(name, loc);
        if (sym != NULL) {
            Tree var_decl = sym->get_tree_decl();
            gcc_assert(!var_decl.is_null());

            if (var_decl.get_type() != integer_type_node) {
                error_at(loc, "variable '%s' does not have integer type", name.c_str());
                sym = SymbolPtr();
            }
        }

        return sym;
    }

    // Returns true if the next token is END, ELSE, or EOF;
    bool Parser::done_end_or_else() {
        const_TokenPtr t = lexer.peek_token();
        return (t->get_id() == END || t->get_id() == ELSE || t->get_id() == END_OF_FILE);
    }

    // Returns true if the next token is END or EOF.
    bool Parser::done_end() {
        const_TokenPtr t = lexer.peek_token();
        return (t->get_id() == END || t->get_id() == END_OF_FILE);
    }

    // Parses expression and ensures it is a variable declaration or array reference.
    Tree Parser::parse_expression_naming_variable() {
        Tree expr = parse_expression();
        if (expr.is_error())
            return expr;

        if (expr.get_tree_code() != VAR_DECL && expr.get_tree_code() != ARRAY_REF
            && expr.get_tree_code() != COMPONENT_REF) {
            error_at(expr.get_locus(), "does not designate a variable, array element or field");
            return Tree::error();
        }

        return expr;
    }

    // Parses expression and ensures it is an assignment expression?
    Tree Parser::parse_lhs_assignment_expression() {
        return parse_expression_naming_variable();
    }

    // Parses type (as in typedef) declaration statement.
    Tree Parser::parse_type_declaration() {
        // type_declaration -> "type" identifier ":" type ";"
        if (!skip_token(TYPE)) {
            skip_after_semicolon();
            return Tree::error();
        }

        // get identifier
        const_TokenPtr identifier = expect_token(IDENTIFIER);
        if (identifier == NULL) {
            skip_after_semicolon();
            return Tree::error();
        }

        // skip colon
        if (!skip_token(COLON)) {
            skip_after_semicolon();
            return Tree::error();
        }

        // get type of expression
        Tree type_tree = parse_type();

        if (type_tree.is_error()) {
            skip_after_semicolon();
            return Tree::error();
        }

        skip_token(SEMICOLON);

        // ensure not already delcared in scope
        if (scope.get_current_mapping().get(identifier->get_str())) {
            error_at(identifier->get_locus(), "name '%s' already declared in this scope",
              identifier->get_str().c_str());
        }

        // create new symbol for typedef and put in mapping for current scope
        SymbolPtr sym(new Symbol(TYPENAME, identifier->get_str()));
        scope.get_current_mapping().insert(sym);

        // build typedef tree
        Tree decl = build_decl(identifier->get_locus(), TYPE_DECL,
          get_identifier(sym->get_name().c_str()), type_tree.get_tree());
        DECL_CONTEXT(decl.get_tree()) = main_fndecl;

        // add type declaration to variable declaration stack
        gcc_assert(!stack_var_decl_chain.empty());
        stack_var_decl_chain.back().append(decl);

        // set symbol's declaration tree to declaration tree
        sym->set_tree_decl(decl);

        // build declaration statement for tree
        Tree stmt = build_tree(DECL_EXPR, identifier->get_locus(), void_type_node, decl);

        return stmt;
    }

    // Parses a record type field declaration.
    Tree Parser::parse_field_declaration(std::vector<std::string>& field_names) {
        // identifier ':' type ';'
        const_TokenPtr identifier = expect_token(IDENTIFIER);
        if (identifier == NULL) {
            skip_after_semicolon();
            return Tree::error();
        }

        skip_token(COLON);

        Tree type = parse_type();

        skip_token(SEMICOLON);

        if (type.is_error())
            return Tree::error();

        // pass vector of fields to avoid repeated field names - error if they exist
        if (std::find(field_names.begin(), field_names.end(), identifier->get_str())
            != field_names.end()) {
            error_at(identifier->get_locus(), "repeated field name");
            return Tree::error();
        }

        field_names.push_back(identifier->get_str());

        // create GENERIC FIELD_DECL tree with name of tree and type
        Tree field_decl = build_decl(identifier->get_locus(), FIELD_DECL,
          get_identifier(identifier->get_str().c_str()), type.get_tree());
        // required for read statement to work on fields
        TREE_ADDRESSABLE(field_decl.get_tree()) = 1;

        return field_decl;
    }

    // Parses a record.
    Tree Parser::parse_record() {
        // "record" field-decl* "end"
        const_TokenPtr record_tok = expect_token(RECORD);
        if (record_tok == NULL) {
            skip_after_semicolon();
            return Tree::error();
        }

        // create empty record type tree
        Tree record_type = make_node(RECORD_TYPE);
        Tree field_list, field_last;
        std::vector<std::string> field_names;

        // parse field declarations inside record until the end token is found
        const_TokenPtr next = lexer.peek_token();
        while (next->get_id() != END) {
            Tree field_decl = parse_field_declaration(field_names);

            if (!field_decl.is_error()) {
                // set field declaration's decl_context to this record type
                DECL_CONTEXT(field_decl.get_tree()) = record_type.get_tree();
                if (field_list.is_null())
                    field_list = field_decl;
                if (!field_last.is_null())
                    // chain fields in record type by using tree_chain
                    TREE_CHAIN(field_last.get_tree()) = field_decl.get_tree();
                field_last = field_decl;
            }

            next = lexer.peek_token();
        }

        skip_token(END);

        // first field sets TYPE_FIELDS attribute of the RECORD_TYPE tree
        TYPE_FIELDS(record_type.get_tree()) = field_list.get_tree();
        // request GCC to layout type in memory
        layout_type(record_type.get_tree());

        return record_type;
    }
}