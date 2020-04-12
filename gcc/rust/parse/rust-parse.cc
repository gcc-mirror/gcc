#include "rust-parse.h"

// instead of gcc includes specified here. Thanks clang-format.
#include "rust-parse-includes.h"

#include <algorithm> // for std::find

namespace Rust {
    // Left binding powers of operations.
    enum binding_powers {
        // Highest priority
        LBP_HIGHEST = 100,

        LBP_PATH = 95,

        LBP_METHOD_CALL = 90,

        LBP_FIELD_EXPR = 85,

        // LBP_DOT = 80, /* method call and field expr have different precedence now */

        LBP_FUNCTION_CALL = 80,
        LBP_ARRAY_REF = LBP_FUNCTION_CALL,

        LBP_QUESTION_MARK = 75, // unary postfix - counts as left

        LBP_UNARY_PLUS = 70,                 // Used only when the null denotation is +
        LBP_UNARY_MINUS = LBP_UNARY_PLUS,    // Used only when the null denotation is -
        LBP_UNARY_ASTERISK = LBP_UNARY_PLUS, // deref operator - unary prefix
        LBP_UNARY_EXCLAM = LBP_UNARY_PLUS,
        LBP_UNARY_AMP = LBP_UNARY_PLUS,
        LBP_UNARY_AMP_MUT = LBP_UNARY_PLUS,

        LBP_AS = 65,

        LBP_MUL = 60,
        LBP_DIV = LBP_MUL,
        LBP_MOD = LBP_MUL,

        LBP_PLUS = 55,
        LBP_MINUS = LBP_PLUS,

        LBP_L_SHIFT = 50,
        LBP_R_SHIFT = LBP_L_SHIFT,

        LBP_AMP = 45,

        LBP_CARET = 40,

        LBP_PIPE = 35,

        LBP_EQUAL = 30,
        LBP_NOT_EQUAL = LBP_EQUAL,
        LBP_SMALLER_THAN = LBP_EQUAL,
        LBP_SMALLER_EQUAL = LBP_EQUAL,
        LBP_GREATER_THAN = LBP_EQUAL,
        LBP_GREATER_EQUAL = LBP_EQUAL,

        LBP_LOGICAL_AND = 25,

        LBP_LOGICAL_OR = 20,

        LBP_DOT_DOT = 15,
        LBP_DOT_DOT_EQ = LBP_DOT_DOT,

        // TODO: note all these assig operators are RIGHT associative!
        LBP_ASSIG = 10,
        LBP_PLUS_ASSIG = LBP_ASSIG,
        LBP_MINUS_ASSIG = LBP_ASSIG,
        LBP_MULT_ASSIG = LBP_ASSIG,
        LBP_DIV_ASSIG = LBP_ASSIG,
        LBP_MOD_ASSIG = LBP_ASSIG,
        LBP_AMP_ASSIG = LBP_ASSIG,
        LBP_PIPE_ASSIG = LBP_ASSIG,
        LBP_CARET_ASSIG = LBP_ASSIG,
        LBP_L_SHIFT_ASSIG = LBP_ASSIG,
        LBP_R_SHIFT_ASSIG = LBP_ASSIG,

        // return, break, and closures as lowest priority?
        LBP_RETURN = 5,
        LBP_BREAK = LBP_RETURN,
        LBP_CLOSURE = LBP_RETURN, // unary prefix operators

#if 0
        // rust precedences
        PREC_CLOSURE = -40,     // used for closures
        PREC_JUMP = -30,        // used for break, continue, return, and yield
        PREC_RANGE = -10,       // used for range (although weird comment in rustc about this)
        PREC_BINOP = FROM_ASSOC_OP, 
        // used for binary operators mentioned below - also cast, colon (type), assign, assign_op
        PREC_PREFIX = 50,       // used for box, address_of, let, unary (again, weird comment on let)
        PREC_POSTFIX = 60,      // used for await, call, method call, field, index, try, inline asm, macro invocation
        PREC_PAREN = 99,        // used for array, repeat, tuple, literal, path, paren, if, while, for, 'loop', match, block, try block, async, struct
        PREC_FORCE_PAREN = 100,
#endif

        // lowest priority
        LBP_LOWEST = 0
    };

    // Checks if Tree has a string type (tree code pointer_type and tree variant char node).
    bool is_string_type(Tree type) {
        // assert node represents a type
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

    // Returns whether the token can start a type (i.e. there is a valid type beginning with the
    // token).
    bool can_tok_start_type(TokenId id) {
        switch (id) {
            case EXCLAM:
            case LEFT_SQUARE:
            case LEFT_ANGLE:
            case UNDERSCORE:
            case ASTERISK:
            case AMP:
            case LIFETIME:
            case IDENTIFIER:
            case SUPER:
            case SELF:
            case SELF_ALIAS:
            case CRATE:
            case DOLLAR_SIGN:
            case SCOPE_RESOLUTION:
            case LEFT_PAREN:
            case FOR:
            case ASYNC:
            case CONST:
            case UNSAFE:
            case EXTERN_TOK:
            case FN_TOK:
            case IMPL:
            case DYN:
            case QUESTION_MARK:
                return true;
            default:
                return false;
        }
    }

    /* Returns whether the token id is (or is likely to be) a right angle bracket. i.e. '>', '>>',
     * '>=' and '>>=' tokens. */
    bool is_right_angle_tok(TokenId id) {
        switch (id) {
            case RIGHT_ANGLE:
            case RIGHT_SHIFT:
            case GREATER_OR_EQUAL:
            case RIGHT_SHIFT_EQ:
                return true;
            default:
                return false;
        }
    }

    // HACK-y special handling for skipping a right angle token at the end of generic arguments.
    bool Parser::skip_generics_right_angle() {
        // HACK: special handling for right shift '>>', greater or equal '>=', and right shift assig
        // '>>='
        const_TokenPtr tok = lexer.peek_token();
        switch (tok->get_id()) {
            case RIGHT_ANGLE:
                // this is good - skip token
                lexer.skip_token();
                return true;
            case RIGHT_SHIFT: {
                /* shit. preferred HACK would be to replace this token in stream with '>', but may not
                 * be possible at this point. */
                // FIXME: ensure locations aren't messed up
                TokenPtr right_angle = Token::make(RIGHT_ANGLE, tok->get_locus() + 1);
                lexer.replace_current_token(right_angle);
                return true;
            }
            case GREATER_OR_EQUAL: {
                // another HACK - replace with equal (as assignment intended, probably)
                /* FIXME: is this even required? how many people wouldn't leave a space? - apparently
                 * rustc has this feature */
                // FIXME: ensure locations aren't messed up
                TokenPtr equal = Token::make(EQUAL, tok->get_locus() + 1);
                lexer.replace_current_token(equal);
                return true;
            }
            case RIGHT_SHIFT_EQ: {
                // another HACK - replace with greater or equal
                // FIXME: again, is this really required? rustc has the feature, though
                // FIXME: ensure locations aren't messed up
                TokenPtr greater_equal = Token::make(GREATER_OR_EQUAL, tok->get_locus() + 1);
                lexer.replace_current_token(greater_equal);
                return true;
            }
            default:
                error_at(tok->get_locus(), "expected '>' at end of generic argument - found '%s'",
                  tok->get_token_description());
                return false;
        }
    }

    /* Gets left binding power for specified token.
     * Not suitable for use at the moment or possibly ever because binding power cannot be purely
     * determined from operator token with Rust grammar - e.g. method call and field access have
     * different left binding powers but the same operator token. */
    int Parser::left_binding_power(const_TokenPtr token) {
        // HACK: called with "peek_token()", so lookahead is "peek_token(1)"
        switch (token->get_id()) {
                /* TODO: issue here - distinguish between method calls and field access somehow?
                    Also would have to distinguish between paths and function calls (:: operator),
                    maybe more stuff. */
                /* Current plan for tackling LBP - don't do it based on token, use lookahead.
                 * Or alternatively, only use Pratt parsing for OperatorExpr and handle other
                 * expressions without it. rustc only considers arithmetic, logical/relational, 'as',
                 * '?=', ranges, colons, and assignment to have operator precedence and associativity
                 * rules applicable. It then has
                 * a separate "ExprPrecedence" that also includes binary operators. */

                // TODO: handle operator overloading - have a function replace the operator?

                /*case DOT:
                    return LBP_DOT;*/

            case SCOPE_RESOLUTION:
                fprintf(stderr, "possible error - looked up LBP of scope resolution operator. should "
                                "be handled elsewhere. \n");
                return LBP_PATH;

            /* Resolved by lookahead HACK that should work with current code. If next token is
             * identifier and token after that isn't parenthesised expression list, it is a field
             * reference. */
            case DOT:
                if (lexer.peek_token(1)->get_id() == IDENTIFIER
                    && lexer.peek_token(2)->get_id() != LEFT_PAREN) {
                    return LBP_FIELD_EXPR;
                }
                return LBP_METHOD_CALL;

            case LEFT_PAREN:
                return LBP_FUNCTION_CALL;

            case LEFT_SQUARE:
                return LBP_ARRAY_REF;

            // postfix question mark (i.e. error propagation expression)
            case QUESTION_MARK:
                return LBP_QUESTION_MARK;

            case AS:
                return LBP_AS;

            case ASTERISK:
                return LBP_MUL;
            case DIV:
                return LBP_DIV;
            case PERCENT:
                return LBP_MOD;

            case PLUS:
                return LBP_PLUS;
            case MINUS:
                return LBP_MINUS;

            case LEFT_SHIFT:
                return LBP_L_SHIFT;
            case RIGHT_SHIFT:
                return LBP_R_SHIFT;

            // binary & operator
            case AMP:
                return LBP_AMP;

            // binary ^ operator
            case CARET:
                return LBP_CARET;

            // binary | operator
            case PIPE:
                return LBP_PIPE;

            case EQUAL_EQUAL:
                return LBP_EQUAL;
            case NOT_EQUAL:
                return LBP_NOT_EQUAL;
            case RIGHT_ANGLE:
                return LBP_GREATER_THAN;
            case GREATER_OR_EQUAL:
                return LBP_GREATER_EQUAL;
            case LEFT_ANGLE:
                return LBP_SMALLER_THAN;
            case LESS_OR_EQUAL:
                return LBP_SMALLER_EQUAL;

            case LOGICAL_AND:
                return LBP_LOGICAL_AND;

            case OR:
                return LBP_LOGICAL_OR;

            case DOT_DOT:
                return LBP_DOT_DOT;

            case DOT_DOT_EQ:
                return LBP_DOT_DOT_EQ;

            case EQUAL:
                return LBP_ASSIG;
            case PLUS_EQ:
                return LBP_PLUS_ASSIG;
            case MINUS_EQ:
                return LBP_MINUS_ASSIG;
            case ASTERISK_EQ:
                return LBP_MULT_ASSIG;
            case DIV_EQ:
                return LBP_DIV_ASSIG;
            case PERCENT_EQ:
                return LBP_MOD_ASSIG;
            case AMP_EQ:
                return LBP_AMP_ASSIG;
            case CARET_EQ:
                return LBP_CARET_ASSIG;
            case LEFT_SHIFT_EQ:
                return LBP_L_SHIFT_ASSIG;
            case RIGHT_SHIFT_EQ:
                return LBP_R_SHIFT_ASSIG;

            // HACK: float literal due to lexer misidentifying a dot then an integer as a float
            case FLOAT_LITERAL:
                return LBP_FIELD_EXPR;
                // field expr is same as tuple expr in precedence, i imagine

            // anything that can't appear in an infix position is given lowest priority
            default:
                return LBP_LOWEST;
        }
    }

    TreeStmtList& Parser::get_current_stmt_list() {
        return stack_stmt_list.back();
    }

    // Parse statements until done (EOF) and append to current stmt list.
    void Parser::parse_statement_seq(bool (Parser::*done)()) {
        // Parse statements until done and append to the current stmt list
        /*while (!(this->*done)()) {
            // get stmt tree for parsed statement
            Tree stmt = parse_statement();
            // append each stmt tree to current stmt list
            get_current_stmt_list().append(stmt);
        }*/
    }

    // Parse "items" until done (EOF) and append to current something list. Seems to be method taken
    // rather than statements in rust.
    /*void Parser::parse_item_seq(bool (Parser::*done)()) {
        // Parse statements until done and append to the current stmt list
        // TODO: fix
        while (!(this->*done)()) {
            // get stmt tree for parsed statement
            Tree item = parse_item();
            // append each stmt tree to current stmt list
            get_current_stmt_list().append(stmt);
        }
    }*/

    // Returns true when current token is EOF.
    bool Parser::done_end_of_file() {
        const_TokenPtr t = lexer.peek_token();
        return (t->get_id() == END_OF_FILE);
    }

    // Entry point - parse entire program (in file) from here.
    void Parser::parse_program() {
        // should be only able to parse decls at this point (scope)?

        // TODO: convert to a crate-based approach? parse_crate()?
        parse_crate();

        // TODO: structural changes - strongly-typed AST instead of Trees?

        // TODO: how much of this is fake main function vs actually required for any function?
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

        // Finish/finalise main function
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

    // Parses a crate (compilation unit) - entry point
    AST::Crate Parser::parse_crate() {
        /* TODO: determine if has utf8bom and shebang. Currently, they are eliminated by the lexing
         * phase.
         * Neither are useful for the compiler anyway, so maybe a better idea would be to eliminate
         * the has_utf8bom and has_shebang variables from the crate data structure. */
        bool has_utf8bom = false;
        bool has_shebang = false;

        // parse inner attributes
        ::std::vector<AST::Attribute> inner_attrs = parse_inner_attributes();

        // parse items
        ::std::vector< ::std::unique_ptr<AST::Item> > items;

        const_TokenPtr t = lexer.peek_token();
        while (t->get_id() != END_OF_FILE) {
            ::std::unique_ptr<AST::Item> item = parse_item(false);
            if (item == NULL) {
                error_at(lexer.peek_token()->get_locus(), "failed to parse item in crate");
                items = ::std::vector< ::std::unique_ptr<AST::Item> >();
                break;
            }

            items.push_back(::std::move(item));

            t = lexer.peek_token();
        }

        return AST::Crate(::std::move(items), ::std::move(inner_attrs), has_utf8bom, has_shebang);
    }

    // Parse a contiguous block of inner attributes.
    ::std::vector<AST::Attribute> Parser::parse_inner_attributes() {
        ::std::vector<AST::Attribute> inner_attributes;

        while (lexer.peek_token()->get_id() == HASH) {
            AST::Attribute inner_attr = parse_inner_attribute();

            // Ensure only valid inner attributes are added to the inner_attributes list
            if (!inner_attr.is_empty()) {
                inner_attributes.push_back(::std::move(inner_attr));
            } else {
                /* If no more valid inner attributes, break out of loop (only contiguous inner
                 * attributes parsed). */
                break;
            }
        }

        return inner_attributes;
    }

    // Parse a single inner attribute.
    AST::Attribute Parser::parse_inner_attribute() {
        if (lexer.peek_token()->get_id() != HASH)
            return AST::Attribute::create_empty();

        lexer.skip_token();

        if (lexer.peek_token()->get_id() != EXCLAM)
            return AST::Attribute::create_empty();

        lexer.skip_token();

        if (lexer.peek_token()->get_id() != LEFT_SQUARE)
            return AST::Attribute::create_empty();

        lexer.skip_token();

        AST::Attribute actual_attribute = parse_attribute_body();

        if (lexer.peek_token()->get_id() != RIGHT_SQUARE)
            return AST::Attribute::create_empty();

        lexer.skip_token();

        return actual_attribute;
    }

    // Parses the body of an attribute (inner or outer).
    AST::Attribute Parser::parse_attribute_body() {
        location_t locus = lexer.peek_token()->get_locus();

        AST::SimplePath attr_path = parse_simple_path();
        // ensure path is valid to parse attribute input
        if (attr_path.is_empty()) {
            error_at(lexer.peek_token()->get_locus(), "empty simple path in attribute");

            // Skip past potential further info in attribute (i.e. attr_input)
            skip_after_end_attribute();
            return AST::Attribute::create_empty();
        }

        ::std::unique_ptr<AST::AttrInput> attr_input = parse_attr_input();
        // AttrInput is allowed to be null, so no checks here

        return AST::Attribute(::std::move(attr_path), ::std::move(attr_input), locus);
    }

    // Parses a SimplePath AST node
    AST::SimplePath Parser::parse_simple_path() {
        bool has_opening_scope_resolution = false;
        location_t locus = UNKNOWN_LOCATION;

        // Checks for opening scope resolution (i.e. global scope fully-qualified path)
        if (lexer.peek_token()->get_id() == SCOPE_RESOLUTION) {
            has_opening_scope_resolution = true;

            locus = lexer.peek_token()->get_locus();

            lexer.skip_token();
        }

        // Parse single required simple path segment
        AST::SimplePathSegment segment = parse_simple_path_segment();

        // get location if not gotten already
        if (locus == UNKNOWN_LOCATION) {
            locus = segment.get_locus();
        }

        ::std::vector<AST::SimplePathSegment> segments;

        // Return empty vector if first, actually required segment is an error
        if (segment.is_error()) {
            return AST::SimplePath::create_empty();
        }

        segments.push_back(segment);

        // Parse all other simple path segments
        while (lexer.peek_token()->get_id() == SCOPE_RESOLUTION) {
            // Skip scope resolution operator
            lexer.skip_token();

            AST::SimplePathSegment new_segment = parse_simple_path_segment();

            // Return path as currently constructed if segment in error state.
            if (new_segment.is_error()) {
                break;
            }
            segments.push_back(new_segment);
        }

        // DEBUG: check for any empty segments
        for (const auto& seg : segments) {
            if (seg.is_error()) {
                fprintf(stderr,
                  "when parsing simple path, somehow empty path segment was not filtered out. Path "
                  "begins with '%s' \n",
                  segments.at(0).as_string().c_str());
            }
        }

        return AST::SimplePath(::std::move(segments), has_opening_scope_resolution, locus);
    }

    // Parses a single SimplePathSegment (does not handle the scope resolution operators)
    AST::SimplePathSegment Parser::parse_simple_path_segment() {
        const_TokenPtr t = lexer.peek_token();
        switch (t->get_id()) {
            case IDENTIFIER:
                lexer.skip_token();

                return AST::SimplePathSegment(t->get_str(), t->get_locus());
            case SUPER:
                lexer.skip_token();

                return AST::SimplePathSegment(::std::string("super"), t->get_locus());
            case SELF:
                lexer.skip_token();

                return AST::SimplePathSegment(::std::string("self"), t->get_locus());
            case CRATE:
                lexer.skip_token();

                return AST::SimplePathSegment(::std::string("crate"), t->get_locus());
            case DOLLAR_SIGN:
                if (lexer.peek_token(1)->get_id() == CRATE) {
                    lexer.skip_token(1);

                    return AST::SimplePathSegment(::std::string("$crate"), t->get_locus());
                }
                gcc_fallthrough();
            default:
                // do nothing but inactivates warning from gcc when compiling
                // could put the error_at thing here but fallthrough (from failing $crate condition)
                // isn't completely obvious if it is.

                // test prevent error
                return AST::SimplePathSegment::create_error();
        }
        gcc_unreachable();
        /*error_at(
          t->get_locus(), "invalid token '%s' in simple path segment", t->get_token_description());*/
        // this is not necessarily an error, e.g. end of path
        // return AST::SimplePathSegment::create_error();
    }

    // Parses a PathIdentSegment - an identifier segment of a non-SimplePath path.
    AST::PathIdentSegment Parser::parse_path_ident_segment() {
        const_TokenPtr t = lexer.peek_token();
        switch (t->get_id()) {
            case IDENTIFIER:
                lexer.skip_token();

                return AST::PathIdentSegment(t->get_str());
            case SUPER:
                lexer.skip_token();

                return AST::PathIdentSegment(::std::string("super"));
            case SELF:
                lexer.skip_token();

                return AST::PathIdentSegment(::std::string("self"));
            case SELF_ALIAS:
                lexer.skip_token();

                return AST::PathIdentSegment(::std::string("Self"));
            case CRATE:
                lexer.skip_token();

                return AST::PathIdentSegment(::std::string("crate"));
            case DOLLAR_SIGN:
                if (lexer.peek_token(1)->get_id() == CRATE) {
                    lexer.skip_token(1);

                    return AST::PathIdentSegment(::std::string("$crate"));
                }
                gcc_fallthrough();
            default:
                // do nothing but inactivates warning from gcc when compiling
                // could put the error_at thing here but fallthrough (from failing $crate condition)
                // isn't completely obvious if it is.

                // test prevent error
                return AST::PathIdentSegment::create_error();
        }
        gcc_unreachable();
        // not necessarily an error
    }

    // Parses an AttrInput AST node (polymorphic, as AttrInput is abstract)
    ::std::unique_ptr<AST::AttrInput> Parser::parse_attr_input() {
        const_TokenPtr t = lexer.peek_token();
        switch (t->get_id()) {
            case LEFT_PAREN:
            case LEFT_SQUARE:
            case LEFT_CURLY: {
                // must be a delimited token tree, so parse that
                ::std::unique_ptr<AST::DelimTokenTree> input_tree(
                  new AST::DelimTokenTree(parse_delim_token_tree()));

                // TODO: potential checks on DelimTokenTree before returning

                return input_tree;
            }
            case EQUAL: {
                // = LiteralExpr
                lexer.skip_token();

                t = lexer.peek_token();

                // Ensure token is a "literal expression" (literally only a literal token of any type)
                if (!t->is_literal()) {
                    error_at(t->get_locus(),
                      "unknown token '%s' in attribute body - literal expected",
                      t->get_token_description());
                    skip_after_end_attribute();
                    return NULL;
                }

                AST::Literal::LitType lit_type = AST::Literal::STRING;
                // Crappy mapping of token type to literal type
                switch (t->get_id()) {
                    case INT_LITERAL:
                        lit_type = AST::Literal::INT;
                        break;
                    case FLOAT_LITERAL:
                        lit_type = AST::Literal::FLOAT;
                        break;
                    case CHAR_LITERAL:
                        lit_type = AST::Literal::CHAR;
                        break;
                    case BYTE_CHAR_LITERAL:
                        lit_type = AST::Literal::BYTE;
                        break;
                    case BYTE_STRING_LITERAL:
                        lit_type = AST::Literal::BYTE_STRING;
                        break;
                    case STRING_LITERAL:
                    default:
                        lit_type = AST::Literal::STRING;
                        break; // TODO: raw string? don't eliminate it from lexer?
                }

                // create actual LiteralExpr
                AST::LiteralExpr lit_expr(t->get_str(), lit_type, t->get_locus());

                ::std::unique_ptr<AST::AttrInputLiteral> attr_input_lit(
                  new AST::AttrInputLiteral(::std::move(lit_expr)));

                // do checks or whatever? none required, really

                // FIXME: shouldn't a skip token be required here?

                return attr_input_lit;
            } break;
            case RIGHT_SQUARE:
                // means AttrInput is missing, which is allowed
                return NULL;
            default:
                error_at(t->get_locus(),
                  "unknown token '%s' in attribute body - attribute input or none expected",
                  t->get_token_description());
                skip_after_end_attribute();
                return NULL;
        }
        gcc_unreachable();
        // TODO: find out how to stop gcc error on "no return value"
    }

    /* Returns true if the token id matches the delimiter type. Note that this only operates for
     * END delimiter tokens. */
    inline bool token_id_matches_delims(TokenId token_id, AST::DelimType delim_type) {
        return ((token_id == RIGHT_PAREN && delim_type == AST::PARENS)
                || (token_id == RIGHT_SQUARE && delim_type == AST::SQUARE)
                || (token_id == RIGHT_CURLY && delim_type == AST::CURLY));
    }

    /* Returns true if the likely result of parsing the next few tokens is a path. Not guaranteed,
     * though, especially in the case of syntax errors. */
    inline bool is_likely_path_next(TokenId next_token_id) {
        switch (next_token_id) {
            case IDENTIFIER:
            case SUPER:
            case SELF:
            case SELF_ALIAS:
            case CRATE:
            // maybe - maybe do extra check. But then requires another TokenId.
            case DOLLAR_SIGN:
            case SCOPE_RESOLUTION:
                return true;
            default:
                return false;
        }
    }

    // Parses a delimited token tree
    AST::DelimTokenTree Parser::parse_delim_token_tree() {
        // DEBUG
        fprintf(stderr, "new delim token tree parsing begun\n");

        const_TokenPtr t = lexer.peek_token();
        lexer.skip_token();
        location_t initial_loc = t->get_locus();

        // save delim type to ensure it is reused later
        AST::DelimType delim_type = AST::PARENS;

        // Map tokens to DelimType
        switch (t->get_id()) {
            case LEFT_PAREN:
                delim_type = AST::PARENS;
                break;
            case LEFT_SQUARE:
                delim_type = AST::SQUARE;
                break;
            case LEFT_CURLY:
                delim_type = AST::CURLY;
                break;
            default:
                error_at(t->get_locus(),
                  "unexpected token '%s' - expecting delimiters (for a delimited token tree)",
                  t->get_token_description());
                return AST::DelimTokenTree::create_empty();
        }

        // parse actual token tree vector - 0 or more
        ::std::vector< ::std::unique_ptr<AST::TokenTree> > token_trees_in_tree;

        // repeat loop until finding the matching delimiter
        t = lexer.peek_token();
        while (!token_id_matches_delims(t->get_id(), delim_type)) {
            ::std::unique_ptr<AST::TokenTree> tok_tree = parse_token_tree();

            if (tok_tree == NULL) {
                // TODO: is this error handling appropriate?
                error_at(t->get_locus(),
                  "failed to parse token tree in delimited token tree - found '%s'",
                  t->get_token_description());
                return AST::DelimTokenTree::create_empty();
            }

            token_trees_in_tree.push_back(::std::move(tok_tree));

            // lexer.skip_token();
            t = lexer.peek_token();
        }

        AST::DelimTokenTree token_tree(delim_type, ::std::move(token_trees_in_tree), initial_loc);

        // parse end delimiters
        t = lexer.peek_token();

        if (token_id_matches_delims(t->get_id(), delim_type)) {
            // tokens match opening delimiter, so skip.
            lexer.skip_token();

            // DEBUG
            fprintf(stderr,
              "finished parsing new delim token tree - peeked token is now '%s' while t is '%s'\n",
              lexer.peek_token()->get_token_description(), t->get_token_description());

            return token_tree;
        } else {
            // tokens don't match opening delimiters, so produce error
            error_at(t->get_locus(),
              "unexpected token '%s' - expecting closing delimiter '%s' (for a delimited token tree)",
              t->get_token_description(),
              (delim_type == AST::PARENS ? ")" : (delim_type == AST::SQUARE ? "]" : "}")));

            /* return empty token tree despite possibly parsing valid token tree - TODO is this a
             * good idea? */
            return AST::DelimTokenTree::create_empty();
        }
    }

    /* Parses a TokenTree syntactical production. This is either a delimited token tree or a
     * non-delimiter token. */
    ::std::unique_ptr<AST::TokenTree> Parser::parse_token_tree() {
        const_TokenPtr t = lexer.peek_token();

        switch (t->get_id()) {
            case LEFT_PAREN:
            case LEFT_SQUARE:
            case LEFT_CURLY:
                // Parse delimited token tree
                // TODO: use move rather than copy constructor
                return ::std::unique_ptr<AST::DelimTokenTree>(
                  new AST::DelimTokenTree(parse_delim_token_tree()));
            case RIGHT_PAREN:
            case RIGHT_SQUARE:
            case RIGHT_CURLY:
                // error - should not be called when this a token
                error_at(t->get_locus(),
                  "unexpected closing delimiter '%s' - token tree requires either paired delimiters "
                  "or non-delimiter tokens",
                  t->get_token_description());
                lexer.skip_token();
                return NULL;
            default:
                // parse token itself as TokenTree
                lexer.skip_token();
                // TODO: fix that token constructor, possibly with c++11 features
                return ::std::unique_ptr<AST::Token>(new AST::Token(t));
        }
    }

    /* Parses a sequence of items within a module or the implicit top-level module in a crate. Note:
     * this is not currently used as parsing an item sequence individually is pretty simple and allows
     * for better error diagnostics and detection. */
    ::std::vector< ::std::unique_ptr<AST::Item> > Parser::parse_items() {
        ::std::vector< ::std::unique_ptr<AST::Item> > items;

        // TODO: replace with do-while loop?
        // infinite loop to save on comparisons (may be a tight loop) - breaks when next item is null
        while (true) {
            ::std::unique_ptr<AST::Item> item = parse_item(false);

            if (item != NULL) {
                items.push_back(::std::move(item));
            } else {
                break;
            }
        }

        return items;
    }

    // Parses a single item
    ::std::unique_ptr<AST::Item> Parser::parse_item(bool called_from_statement) {
        // has a "called_from_statement" parameter for better error message handling

        // parse outer attributes for item
        ::std::vector<AST::Attribute> outer_attrs = parse_outer_attributes();

        // TODO: decide how to deal with VisItem vs MacroItem dichotomy
        // best current solution: catch all keywords that would imply a VisItem in a switch and have
        // MacroItem as a last resort

        const_TokenPtr t = lexer.peek_token();

        switch (t->get_id()) {
            case END_OF_FILE:
                // not necessarily an error
                return NULL;
            case PUB:
            case MOD:
            case EXTERN_TOK:
            case USE:
            case FN_TOK:
            case TYPE:
            case STRUCT_TOK:
            case ENUM_TOK:
            case CONST:
            case STATIC_TOK:
            case TRAIT:
            case IMPL:
            /* TODO: implement union keyword but not really because of context-dependence
             * crappy hack way to parse a union written below to separate it from the good code. */
            // case UNION:
            case UNSAFE: // maybe - unsafe traits are a thing
                // if any of these (should be all possible VisItem prefixes), parse a VisItem
                return parse_vis_item(::std::move(outer_attrs));
                break;
            case SUPER:
            case SELF:
            case CRATE:
            case DOLLAR_SIGN:
                // almost certainly macro invocation semi
                return parse_macro_item(::std::move(outer_attrs));
                break;
            // crappy hack to do union "keyword"
            case IDENTIFIER:
                // TODO: ensure std::string and literal comparison works
                if (t->get_str() == "union") {
                    return parse_vis_item(::std::move(outer_attrs));
                    // or should this go straight to parsing union?
                } else if (t->get_str() == "macro_rules") {
                    // macro_rules! macro item
                    return parse_macro_item(::std::move(outer_attrs));
                } else if (lexer.peek_token(1)->get_id() == SCOPE_RESOLUTION
                           || lexer.peek_token(1)->get_id() == EXCLAM) {
                    // path (probably) or macro invocation, so probably a macro invocation semi
                    return parse_macro_item(::std::move(outer_attrs));
                }
                gcc_fallthrough();
                // TODO: find out how to disable gcc "implicit fallthrough" warning
            default:
                // otherwise unrecognised
                // return parse_macro_item(::std::move(outer_attrs));
                error_at(t->get_locus(), "unrecognised token '%s' for start of %s",
                  t->get_token_description(), called_from_statement ? "statement" : "item");
                // skip somewhere?
                return NULL;
                break;
        }
    }

    // Parses a contiguous block of outer attributes.
    ::std::vector<AST::Attribute> Parser::parse_outer_attributes() {
        ::std::vector<AST::Attribute> outer_attributes;

        while (lexer.peek_token()->get_id() == HASH) {
            AST::Attribute outer_attr = parse_outer_attribute();

            // Ensure only valid outer attributes are added to the outer_attributes list
            if (!outer_attr.is_empty()) {
                outer_attributes.push_back(::std::move(outer_attr));
            } else {
                /* If no more valid outer attributes, break out of loop (only contiguous outer
                 * attributes parsed). */
                break;
            }
        }

        return outer_attributes;

        // TODO: this shares basically all code with parse_inner_attributes except function call
        // find way of making it more modular?
    }

    // Parse a single outer attribute.
    AST::Attribute Parser::parse_outer_attribute() {
        /* OuterAttribute -> '#' '[' Attr ']' */

        if (lexer.peek_token()->get_id() != HASH)
            return AST::Attribute::create_empty();

        lexer.skip_token();

        TokenId id = lexer.peek_token()->get_id();
        if (id != LEFT_SQUARE) {
            if (id == EXCLAM) {
                // this is inner attribute syntax, so throw error
                error_at(lexer.peek_token()->get_locus(),
                  "token '!' found, indicating inner attribute definition. Inner attributes are not "
                  "possible at this location.");
            } // TODO: are there any cases where this wouldn't be an error?
            return AST::Attribute::create_empty();
        }

        lexer.skip_token();

        AST::Attribute actual_attribute = parse_attribute_body();

        if (lexer.peek_token()->get_id() != RIGHT_SQUARE)
            return AST::Attribute::create_empty();

        lexer.skip_token();

        return actual_attribute;
    }

    // Parses a VisItem (item that can have non-default visibility).
    ::std::unique_ptr<AST::VisItem> Parser::parse_vis_item(
      ::std::vector<AST::Attribute> outer_attrs) {
        // parse visibility, which may or may not exist
        AST::Visibility vis = parse_visibility();

        // select VisItem to create depending on keyword
        const_TokenPtr t = lexer.peek_token();

        switch (t->get_id()) {
            case MOD:
                return parse_module(::std::move(vis), ::std::move(outer_attrs));
            case EXTERN_TOK:
                // lookahead to resolve syntactical production
                t = lexer.peek_token(1);

                switch (t->get_id()) {
                    case CRATE:
                        return parse_extern_crate(::std::move(vis), ::std::move(outer_attrs));
                    case FN_TOK: // extern function
                        return parse_function(::std::move(vis), ::std::move(outer_attrs));
                    case LEFT_CURLY: // extern block
                        return parse_extern_block(::std::move(vis), ::std::move(outer_attrs));
                    case STRING_LITERAL: // for specifying extern ABI
                        // could be extern block or extern function, so more lookahead
                        t = lexer.peek_token(2);

                        switch (t->get_id()) {
                            case FN_TOK:
                                return parse_function(::std::move(vis), ::std::move(outer_attrs));
                            case LEFT_CURLY:
                                return parse_extern_block(::std::move(vis), ::std::move(outer_attrs));
                            default:
                                error_at(t->get_locus(),
                                  "unexpected token '%s' in some sort of extern production",
                                  t->get_token_description());
                                lexer.skip_token(2); // TODO: is this right thing to do?
                                return NULL;
                        }
                    default:
                        error_at(t->get_locus(),
                          "unexpected token '%s' in some sort of extern production",
                          t->get_token_description());
                        lexer.skip_token(1); // TODO: is this right thing to do?
                        return NULL;
                }
            case USE:
                return parse_use_decl(::std::move(vis), ::std::move(outer_attrs));
            case FN_TOK:
                return parse_function(::std::move(vis), ::std::move(outer_attrs));
            case TYPE:
                return parse_type_alias(::std::move(vis), ::std::move(outer_attrs));
            case STRUCT_TOK:
                return parse_struct(::std::move(vis), ::std::move(outer_attrs));
            case ENUM_TOK:
                return parse_enum(::std::move(vis), ::std::move(outer_attrs));
            // TODO: implement union keyword but not really because of context-dependence
            // case UNION:
            // crappy hack to do union "keyword"
            case IDENTIFIER:
                // TODO: ensure std::string and literal comparison works
                if (t->get_str() == "union") {
                    return parse_union(::std::move(vis), ::std::move(outer_attrs));
                    // or should item switch go straight to parsing union?
                } else {
                    break;
                }
            case CONST:
                // lookahead to resolve syntactical production
                t = lexer.peek_token(1);

                switch (t->get_id()) {
                    case IDENTIFIER:
                    case UNDERSCORE:
                        return parse_const_item(::std::move(vis), ::std::move(outer_attrs));
                    case UNSAFE:
                    case EXTERN_TOK:
                    case FN_TOK:
                        return parse_function(::std::move(vis), ::std::move(outer_attrs));
                    default:
                        error_at(t->get_locus(),
                          "unexpected token '%s' in some sort of const production",
                          t->get_token_description());
                        lexer.skip_token(1); // TODO: is this right thing to do?
                        return NULL;
                }
            case STATIC_TOK:
                return parse_static_item(::std::move(vis), ::std::move(outer_attrs));
            case TRAIT:
                return parse_trait(::std::move(vis), ::std::move(outer_attrs));
            case IMPL:
                return parse_impl(::std::move(vis), ::std::move(outer_attrs));
            case UNSAFE: // unsafe traits, unsafe functions, unsafe impls (trait impls),
                // lookahead to resolve syntactical production
                t = lexer.peek_token(1);

                switch (t->get_id()) {
                    case TRAIT:
                        return parse_trait(::std::move(vis), ::std::move(outer_attrs));
                    case EXTERN_TOK:
                    case FN_TOK:
                        return parse_function(::std::move(vis), ::std::move(outer_attrs));
                    case IMPL:
                        return parse_impl(::std::move(vis), ::std::move(outer_attrs));
                    default:
                        error_at(t->get_locus(),
                          "unexpected token '%s' in some sort of unsafe production",
                          t->get_token_description());
                        lexer.skip_token(1); // TODO: is this right thing to do?
                        return NULL;
                }
            default:
                // otherwise vis item clearly doesn't exist, which is not an error
                // has a catch-all post-switch return to allow other breaks to occur
                break;
        }
        return NULL;
    }

    // Parses a MacroItem (either a MacroInvocationSemi or MacroRulesDefinition).
    ::std::unique_ptr<AST::MacroItem> Parser::parse_macro_item(
      ::std::vector<AST::Attribute> outer_attrs) {
        const_TokenPtr t = lexer.peek_token();

        /* dodgy way of detecting macro due to weird context-dependence thing. probably can be
         * improved */
        // TODO: ensure that string compare works properly
        if (t->get_id() == IDENTIFIER && t->get_str() == ::std::string("macro_rules")) {
            return parse_macro_rules_def(::std::move(outer_attrs));
        } else {
            // DEBUG: TODO: remove
            fprintf(stderr, "DEBUG - parse_macro_item called and token is not macro_rules");
            if (t->get_id() == IDENTIFIER) {
                fprintf(stderr,
                  "just add to last error: token is not macro_rules and is instead '%s'",
                  t->get_str().c_str());
            } else {
                fprintf(stderr,
                  "just add to last error: token is not macro_rules and is not an identifier either "
                  "- it is '%s'",
                  t->get_token_description());
            }

            return parse_macro_invocation_semi(::std::move(outer_attrs));
        }
    }

    // Parses a macro rules definition syntax extension whatever thing.
    ::std::unique_ptr<AST::MacroRulesDefinition> Parser::parse_macro_rules_def(
      ::std::vector<AST::Attribute> outer_attrs) {
        // ensure that first token is identifier saying "macro_rules"
        const_TokenPtr t = lexer.peek_token();
        if (t->get_id() != IDENTIFIER || t->get_str() != "macro_rules") {
            error_at(t->get_locus(), "macro rules definition does not start with 'macro_rules'");
            // skip after somewhere?
            return NULL;
        }
        lexer.skip_token();
        location_t macro_locus = t->get_locus();

        if (!skip_token(EXCLAM)) {
            // skip after somewhere?
            return NULL;
        }

        // parse macro name
        const_TokenPtr ident_tok = expect_token(IDENTIFIER);
        if (ident_tok == NULL) {
            return NULL;
        }
        Identifier rule_name = ident_tok->get_str();

        // DEBUG
        fprintf(stderr, "in macro rules def, about to parse parens.\n");

        // save delim type to ensure it is reused later
        AST::DelimType delim_type = AST::PARENS;

        // Map tokens to DelimType
        t = lexer.peek_token();
        switch (t->get_id()) {
            case LEFT_PAREN:
                delim_type = AST::PARENS;
                break;
            case LEFT_SQUARE:
                delim_type = AST::SQUARE;
                break;
            case LEFT_CURLY:
                delim_type = AST::CURLY;
                break;
            default:
                error_at(t->get_locus(),
                  "unexpected token '%s' - expecting delimiters (for a macro rules definition)",
                  t->get_token_description());
                return NULL;
        }
        lexer.skip_token();

        // parse actual macro rules
        ::std::vector<AST::MacroRule> macro_rules;

        // must be at least one macro rule, so parse it
        AST::MacroRule initial_rule = parse_macro_rule();
        if (initial_rule.is_error()) {
            error_at(lexer.peek_token()->get_locus(),
              "required first macro rule in macro rules definition could not be parsed");
            // skip after somewhere?
            return NULL;
        }
        macro_rules.push_back(::std::move(initial_rule));

        // DEBUG
        fprintf(stderr, "successfully pushed back initial macro rule\n");

        t = lexer.peek_token();
        // parse macro rules
        while (t->get_id() == SEMICOLON) {
            // skip semicolon
            lexer.skip_token();

            // don't parse if end of macro rules
            if (token_id_matches_delims(lexer.peek_token()->get_id(), delim_type)) {
                // DEBUG
                fprintf(stderr, "broke out of parsing macro rules loop due to finding delim\n");

                break;
            }

            // try to parse next rule
            AST::MacroRule rule = parse_macro_rule();
            if (rule.is_error()) {
                error_at(lexer.peek_token()->get_locus(),
                  "failed to parse macro rule in macro rules definition");
                return NULL;
            }

            macro_rules.push_back(::std::move(rule));

            // DEBUG
            fprintf(stderr, "successfully pushed back another macro rule\n");

            t = lexer.peek_token();
        }

        // parse end delimiters
        t = lexer.peek_token();
        if (token_id_matches_delims(t->get_id(), delim_type)) {
            // tokens match opening delimiter, so skip.
            lexer.skip_token();

            if (delim_type != AST::CURLY) {
                // skip semicolon at end of non-curly macro definitions
                if (!skip_token(SEMICOLON)) {
                    // as this is the end, allow recovery (probably) - may change
                    return ::std::unique_ptr<AST::MacroRulesDefinition>(
                      new AST::MacroRulesDefinition(::std::move(rule_name), delim_type,
                        ::std::move(macro_rules), ::std::move(outer_attrs), macro_locus));
                }
            }

            return ::std::unique_ptr<AST::MacroRulesDefinition>(
              new AST::MacroRulesDefinition(::std::move(rule_name), delim_type,
                ::std::move(macro_rules), ::std::move(outer_attrs), macro_locus));
        } else {
            // tokens don't match opening delimiters, so produce error
            error_at(t->get_locus(),
              "unexpected token '%s' - expecting closing delimiter '%s' (for a macro rules "
              "definition)",
              t->get_token_description(),
              (delim_type == AST::PARENS ? ")" : (delim_type == AST::SQUARE ? "]" : "}")));

            /* return empty macro definiton despite possibly parsing mostly valid one - TODO is this
             * a good idea? */
            return NULL;
        }
    }

    // Parses a semi-coloned (except for full block) macro invocation item.
    ::std::unique_ptr<AST::MacroInvocationSemi> Parser::parse_macro_invocation_semi(
      ::std::vector<AST::Attribute> outer_attrs) {
        location_t macro_locus = lexer.peek_token()->get_locus();
        AST::SimplePath path = parse_simple_path();

        if (!skip_token(EXCLAM)) {
            // skip after somewhere?
            return NULL;
        }

        // save delim type to ensure it is reused later
        AST::DelimType delim_type = AST::PARENS;

        // Map tokens to DelimType
        const_TokenPtr t = lexer.peek_token();
        switch (t->get_id()) {
            case LEFT_PAREN:
                delim_type = AST::PARENS;
                break;
            case LEFT_SQUARE:
                delim_type = AST::SQUARE;
                break;
            case LEFT_CURLY:
                delim_type = AST::CURLY;
                break;
            default:
                error_at(t->get_locus(),
                  "unexpected token '%s' - expecting delimiters (for a macro invocation semi body)",
                  t->get_token_description());
                return NULL;
        }
        lexer.skip_token();

        // parse actual token trees
        ::std::vector< ::std::unique_ptr<AST::TokenTree> > token_trees;

        t = lexer.peek_token();
        // parse token trees until the initial delimiter token is found again
        while (!token_id_matches_delims(t->get_id(), delim_type)) {
            ::std::unique_ptr<AST::TokenTree> tree = parse_token_tree();

            if (tree == NULL) {
                error_at(t->get_locus(),
                  "failed to parse token tree for macro invocation semi - found '%s'",
                  t->get_token_description());
                return NULL;
            }

            token_trees.push_back(::std::move(tree));

            t = lexer.peek_token();
        }

        // parse end delimiters
        t = lexer.peek_token();
        if (token_id_matches_delims(t->get_id(), delim_type)) {
            // tokens match opening delimiter, so skip.
            lexer.skip_token();

            if (delim_type != AST::CURLY) {
                // skip semicolon at end of non-curly macro invocation semis
                if (!skip_token(SEMICOLON)) {
                    // as this is the end, allow recovery (probably) - may change
                    return ::std::unique_ptr<AST::MacroInvocationSemi>(
                      new AST::MacroInvocationSemi(::std::move(path), delim_type,
                        ::std::move(token_trees), ::std::move(outer_attrs), macro_locus));
                }
            }

            // DEBUG:
            fprintf(stderr, "skipped token is '%s', next token (current peek) is '%s'\n",
              t->get_token_description(), lexer.peek_token()->get_token_description());

            return ::std::unique_ptr<AST::MacroInvocationSemi>(
              new AST::MacroInvocationSemi(::std::move(path), delim_type, ::std::move(token_trees),
                ::std::move(outer_attrs), macro_locus));
        } else {
            // tokens don't match opening delimiters, so produce error
            error_at(t->get_locus(),
              "unexpected token '%s' - expecting closing delimiter '%s' (for a macro invocation "
              "semi)",
              t->get_token_description(),
              (delim_type == AST::PARENS ? ")" : (delim_type == AST::SQUARE ? "]" : "}")));

            /* return empty macro invocation despite possibly parsing mostly valid one - TODO is this
             * a good idea? */
            return NULL;
        }
    }

    // Parses a non-semicoloned macro invocation (i.e. as pattern or expression).
    ::std::unique_ptr<AST::MacroInvocation> Parser::parse_macro_invocation(
      ::std::vector<AST::Attribute> outer_attrs) {
        // parse macro path
        AST::SimplePath macro_path = parse_simple_path();
        if (macro_path.is_empty()) {
            error_at(lexer.peek_token()->get_locus(), "failed to parse macro invocation path");
            // skip?
            return NULL;
        }

        if (!skip_token(EXCLAM)) {
            // skip after somewhere?
            return NULL;
        }

        // parse internal delim token tree
        AST::DelimTokenTree delim_tok_tree = parse_delim_token_tree();

        location_t macro_locus = macro_path.get_locus();

        return ::std::unique_ptr<AST::MacroInvocation>(
          new AST::MacroInvocation(::std::move(macro_path), ::std::move(delim_tok_tree),
            ::std::move(outer_attrs), macro_locus));
    }

    // Parses a macro rule definition - does not parse semicolons.
    AST::MacroRule Parser::parse_macro_rule() {
        // DEBUG
        fprintf(stderr, "begun parsing macro rule\n");

        // parse macro matcher
        AST::MacroMatcher matcher = parse_macro_matcher();

        // DEBUG
        fprintf(stderr, "managed to get past parsing macro matcher\n");

        if (matcher.is_error()) {
            return AST::MacroRule::create_error();
        }

        // DEBUG
        fprintf(stderr, "successfully parsed macro matcher\n");

        if (!skip_token(MATCH_ARROW)) {
            // skip after somewhere?
            return AST::MacroRule::create_error();
        }

        // DEBUG
        fprintf(stderr, "successfully skipped match arrow\n");

        // parse transcriber (this is just a delim token tree)
        AST::DelimTokenTree transcribe_tree = parse_delim_token_tree();

        // DEBUG
        fprintf(stderr, "successfully parsed transcribe tree\n");

        AST::MacroTranscriber transcriber(::std::move(transcribe_tree));

        // DEBUG
        fprintf(stderr, "successfully parsed macro transcriber - returning macro rule\n");

        return AST::MacroRule(::std::move(matcher), ::std::move(transcriber));
    }

    // Parses a macro matcher (part of a macro rule definition).
    AST::MacroMatcher Parser::parse_macro_matcher() {
        // save delim type to ensure it is reused later
        AST::DelimType delim_type = AST::PARENS;

        // DEBUG
        fprintf(stderr, "begun parsing macro matcher\n");

        // Map tokens to DelimType
        const_TokenPtr t = lexer.peek_token();
        switch (t->get_id()) {
            case LEFT_PAREN:
                delim_type = AST::PARENS;
                break;
            case LEFT_SQUARE:
                delim_type = AST::SQUARE;
                break;
            case LEFT_CURLY:
                delim_type = AST::CURLY;
                break;
            default:
                error_at(t->get_locus(),
                  "unexpected token '%s' - expecting delimiters (for a macro matcher)",
                  t->get_token_description());
                return AST::MacroMatcher::create_error();
        }
        lexer.skip_token();

        // parse actual macro matches
        ::std::vector< ::std::unique_ptr<AST::MacroMatch> > matches;

        t = lexer.peek_token();
        // parse token trees until the initial delimiter token is found again
        while (!token_id_matches_delims(t->get_id(), delim_type)) {
            ::std::unique_ptr<AST::MacroMatch> match = parse_macro_match();

            if (match == NULL) {
                error_at(t->get_locus(), "failed to parse macro match for macro matcher - found '%s'",
                  t->get_token_description());
                return AST::MacroMatcher::create_error();
            }

            matches.push_back(::std::move(match));

            // DEBUG
            fprintf(stderr, "pushed back a match in macro matcher\n");

            t = lexer.peek_token();
        }

        // parse end delimiters
        t = lexer.peek_token();
        if (token_id_matches_delims(t->get_id(), delim_type)) {
            // tokens match opening delimiter, so skip.
            lexer.skip_token();

            return AST::MacroMatcher(delim_type, ::std::move(matches));
        } else {
            // tokens don't match opening delimiters, so produce error
            error_at(t->get_locus(),
              "unexpected token '%s' - expecting closing delimiter '%s' (for a macro matcher)",
              t->get_token_description(),
              (delim_type == AST::PARENS ? ")" : (delim_type == AST::SQUARE ? "]" : "}")));

            /* return error macro matcher despite possibly parsing mostly correct one? TODO is this
             * the best idea? */
            return AST::MacroMatcher::create_error();
        }
    }

    // Parses a macro match (syntax match inside a matcher in a macro rule).
    ::std::unique_ptr<AST::MacroMatch> Parser::parse_macro_match() {
        // branch based on token available
        const_TokenPtr t = lexer.peek_token();
        switch (t->get_id()) {
            case LEFT_PAREN:
            case LEFT_SQUARE:
            case LEFT_CURLY: {
                // must be macro matcher as delimited
                AST::MacroMatcher matcher = parse_macro_matcher();
                if (matcher.is_error()) {
                    error_at(lexer.peek_token()->get_locus(),
                      "failed to parse macro matcher in macro match");
                    return NULL;
                }
                return ::std::unique_ptr<AST::MacroMatcher>(
                  new AST::MacroMatcher(::std::move(matcher)));
            }
            case DOLLAR_SIGN: {
                // have to do more lookahead to determine if fragment or repetition
                const_TokenPtr t2 = lexer.peek_token(1);
                switch (t2->get_id()) {
                    case IDENTIFIER:
                        // macro fragment
                        return parse_macro_match_fragment();
                    case LEFT_PAREN:
                        // macro repetition
                        return parse_macro_match_repetition();
                    default:
                        // error: unrecognised
                        error_at(t2->get_locus(),
                          "unrecognised token combination '$%s' at start of macro match - did you "
                          "mean '$identifier' or '$('?",
                          t2->get_token_description());
                        // skip somewhere?
                        return NULL;
                }
            }
            case RIGHT_PAREN:
            case RIGHT_SQUARE:
            case RIGHT_CURLY:
                // not allowed
                error_at(t->get_locus(),
                  "closing delimiters like '%s' are not allowed at the start of a macro match",
                  t->get_token_description());
                // skip somewhere?
                return NULL;
            default:
                // just the token
                lexer.skip_token();
                return ::std::unique_ptr<AST::Token>(new AST::Token(t));
        }
    }

    // Parses a fragment macro match.
    ::std::unique_ptr<AST::MacroMatchFragment> Parser::parse_macro_match_fragment() {
        skip_token(DOLLAR_SIGN);

        const_TokenPtr ident_tok = expect_token(IDENTIFIER);
        if (ident_tok == NULL) {
            error_at(lexer.peek_token()->get_locus(), "missing identifier in macro match fragment");
            return NULL;
        }
        Identifier ident = ident_tok->get_str();

        if (!skip_token(COLON)) {
            // skip after somewhere?
            return NULL;
        }

        // get MacroFragSpec for macro
        const_TokenPtr t = expect_token(IDENTIFIER);
        AST::MacroFragSpec frag = AST::get_frag_spec_from_str(t->get_str());
        if (frag == AST::INVALID) {
            error_at(t->get_locus(), "invalid fragment specifier '%s' in fragment macro match",
              t->get_str().c_str());
            return NULL;
        }

        return ::std::unique_ptr<AST::MacroMatchFragment>(
          new AST::MacroMatchFragment(::std::move(ident), frag));
    }

    // Parses a repetition macro match.
    ::std::unique_ptr<AST::MacroMatchRepetition> Parser::parse_macro_match_repetition() {
        skip_token(DOLLAR_SIGN);
        skip_token(LEFT_PAREN);

        ::std::vector< ::std::unique_ptr<AST::MacroMatch> > matches;

        // parse required first macro match
        ::std::unique_ptr<AST::MacroMatch> initial_match = parse_macro_match();
        if (initial_match == NULL) {
            error_at(lexer.peek_token()->get_locus(),
              "could not parse required first macro match in macro match repetition");
            // skip after somewhere?
            return NULL;
        }
        matches.push_back(::std::move(initial_match));

        // parse optional later macro matches
        const_TokenPtr t = lexer.peek_token();
        while (t->get_id() != RIGHT_PAREN) {
            ::std::unique_ptr<AST::MacroMatch> match = parse_macro_match();

            if (match == NULL) {
                error_at(lexer.peek_token()->get_locus(),
                  "failed to parse macro match in macro match repetition");
                return NULL;
            }

            matches.push_back(::std::move(match));

            t = lexer.peek_token();
        }

        if (!skip_token(RIGHT_PAREN)) {
            // skip after somewhere?
            return NULL;
        }

        t = lexer.peek_token();
        // see if separator token exists
        ::std::unique_ptr<AST::Token> separator = NULL;
        switch (t->get_id()) {
            // repetition operators
            case ASTERISK:
            case PLUS:
            case QUESTION_MARK:
            // delimiters
            case LEFT_PAREN:
            case LEFT_CURLY:
            case LEFT_SQUARE:
            case RIGHT_PAREN:
            case RIGHT_CURLY:
            case RIGHT_SQUARE:
                // separator does not exist, so still null and don't skip token
                break;
            default:
                // separator does exist
                separator = ::std::unique_ptr<AST::Token>(new AST::Token(t));
                lexer.skip_token();
                break;
        }

        // parse repetition operator
        t = lexer.peek_token();
        AST::MacroMatchRepetition::MacroRepOp op = AST::MacroMatchRepetition::ASTERISK;
        switch (t->get_id()) {
            case ASTERISK:
                op = AST::MacroMatchRepetition::ASTERISK;
                lexer.skip_token();
                break;
            case PLUS:
                op = AST::MacroMatchRepetition::PLUS;
                lexer.skip_token();
                break;
            case QUESTION_MARK:
                op = AST::MacroMatchRepetition::QUESTION_MARK;
                lexer.skip_token();
                break;
            default:
                error_at(t->get_locus(),
                  "expected macro repetition operator ('*', '+', or '?') in macro match - found '%s'",
                  t->get_token_description());
                // skip after somewhere?
                return NULL;
        }

        return ::std::unique_ptr<AST::MacroMatchRepetition>(
          new AST::MacroMatchRepetition(::std::move(matches), op, ::std::move(separator)));
    }

    // Parses a visibility syntactical production (i.e. creating a non-default visibility)
    AST::Visibility Parser::parse_visibility() {
        // check for no visibility
        if (lexer.peek_token()->get_id() != PUB) {
            return AST::Visibility::create_error();
        }

        lexer.skip_token();

        // create simple pub visibility if no parentheses
        if (lexer.peek_token()->get_id() != LEFT_PAREN) {
            return AST::Visibility::create_public();
            // or whatever
        }

        lexer.skip_token();

        const_TokenPtr t = lexer.peek_token();

        switch (t->get_id()) {
            case CRATE:
                lexer.skip_token();

                skip_token(RIGHT_PAREN);

                return AST::Visibility::create_crate();
            case SELF:
                lexer.skip_token();

                skip_token(RIGHT_PAREN);

                return AST::Visibility::create_self();
            case SUPER:
                lexer.skip_token();

                skip_token(RIGHT_PAREN);

                return AST::Visibility::create_super();
            case IN: {
                lexer.skip_token();

                // parse the "in" path as well
                AST::SimplePath path = parse_simple_path();
                if (path.is_empty()) {
                    error_at(
                      lexer.peek_token()->get_locus(), "missing path in pub(in path) visibility");
                    // skip after somewhere?
                    return AST::Visibility::create_error();
                }

                skip_token(RIGHT_PAREN);

                return AST::Visibility::create_in_path(::std::move(path));
            }
            default:
                error_at(
                  t->get_locus(), "unexpected token '%s' in visibility", t->get_token_description());
                lexer.skip_token();
                return AST::Visibility::create_error();
        }
    }

    // Parses a module - either a bodied module or a module defined in another file.
    ::std::unique_ptr<AST::Module> Parser::parse_module(
      AST::Visibility vis, ::std::vector<AST::Attribute> outer_attrs) {
        location_t locus = lexer.peek_token()->get_locus();
        skip_token(MOD);

        const_TokenPtr module_name = expect_token(IDENTIFIER);
        if (module_name == NULL) {
            return NULL;
        }
        Identifier name = module_name->get_str();

        const_TokenPtr t = lexer.peek_token();

        switch (t->get_id()) {
            case SEMICOLON:
                lexer.skip_token();

                return ::std::unique_ptr<AST::ModuleNoBody>(new AST::ModuleNoBody(::std::move(name),
                  ::std::move(vis), ::std::move(outer_attrs), locus)); // module name?
            case LEFT_CURLY: {
                lexer.skip_token();

                // parse inner attributes
                ::std::vector<AST::Attribute> inner_attrs = parse_inner_attributes();

                // parse items
                ::std::vector< ::std::unique_ptr<AST::Item> > items;
                const_TokenPtr tok = lexer.peek_token();
                while (tok->get_id() != RIGHT_CURLY) {
                    ::std::unique_ptr<AST::Item> item = parse_item(false);
                    if (item == NULL) {
                        error_at(tok->get_locus(), "failed to parse item in module");
                        return NULL;
                    }

                    items.push_back(::std::move(item));

                    tok = lexer.peek_token();
                }

                if (!skip_token(RIGHT_CURLY)) {
                    // skip somewhere?
                    return NULL;
                }

                return ::std::unique_ptr<AST::ModuleBodied>(new AST::ModuleBodied(::std::move(name),
                  locus, ::std::move(items), ::std::move(vis), ::std::move(inner_attrs),
                  ::std::move(outer_attrs))); // module name?
            }
            default:
                error_at(t->get_locus(),
                  "unexpected token '%s' in module declaration/definition item",
                  t->get_token_description());
                lexer.skip_token();
                return NULL;
        }
    }

    // Parses an extern crate declaration (dependency on external crate)
    ::std::unique_ptr<AST::ExternCrate> Parser::parse_extern_crate(
      AST::Visibility vis, ::std::vector<AST::Attribute> outer_attrs) {
        location_t locus = lexer.peek_token()->get_locus();
        if (!skip_token(EXTERN_TOK)) {
            skip_after_semicolon();
            return NULL;
        }

        if (!skip_token(CRATE)) {
            skip_after_semicolon();
            return NULL;
        }

        /* parse crate reference name - this has its own syntactical rule in reference but seems
         * to not be used elsewhere, so i'm putting it here */
        const_TokenPtr crate_name_tok = lexer.peek_token();
        ::std::string crate_name;

        switch (crate_name_tok->get_id()) {
            case IDENTIFIER:
                crate_name = crate_name_tok->get_str();
                lexer.skip_token();
                break;
            case SELF:
                crate_name = ::std::string("self");
                lexer.skip_token();
                break;
            default:
                error_at(crate_name_tok->get_locus(),
                  "expecting crate name (identifier or 'self'), found '%s'",
                  crate_name_tok->get_token_description());
                skip_after_semicolon();
                return NULL;
        }

        // don't parse as clause if it doesn't exist
        if (lexer.peek_token()->get_id() == SEMICOLON) {
            lexer.skip_token();

            return ::std::unique_ptr<AST::ExternCrate>(new AST::ExternCrate(
              ::std::move(crate_name), ::std::move(vis), ::std::move(outer_attrs), locus));
        }

        /* parse as clause - this also has its own syntactical rule in reference and also seems to
         * not be used elsewhere, so including here again. */
        if (!skip_token(AS)) {
            skip_after_semicolon();
            return NULL;
        }

        const_TokenPtr as_name_tok = lexer.peek_token();
        ::std::string as_name;

        switch (as_name_tok->get_id()) {
            case IDENTIFIER:
                as_name = as_name_tok->get_str();
                lexer.skip_token();
                break;
            case UNDERSCORE:
                as_name = ::std::string("_");
                lexer.skip_token();
                break;
            default:
                error_at(as_name_tok->get_locus(),
                  "expecting as clause name (identifier or '_'), found '%s'",
                  as_name_tok->get_token_description());
                skip_after_semicolon();
                return NULL;
        }

        if (!skip_token(SEMICOLON)) {
            skip_after_semicolon();
            return NULL;
        }

        return ::std::unique_ptr<AST::ExternCrate>(new AST::ExternCrate(::std::move(crate_name),
          ::std::move(vis), ::std::move(outer_attrs), locus, ::std::move(as_name)));
    }

    // Parses a use declaration.
    ::std::unique_ptr<AST::UseDeclaration> Parser::parse_use_decl(
      AST::Visibility vis, ::std::vector<AST::Attribute> outer_attrs) {
        location_t locus = lexer.peek_token()->get_locus();
        if (!skip_token(USE)) {
            skip_after_semicolon();
            return NULL;
        }

        // parse use tree, which is required
        ::std::unique_ptr<AST::UseTree> use_tree = parse_use_tree();
        if (use_tree == NULL) {
            error_at(lexer.peek_token()->get_locus(), "could not parse use tree in use declaration");
            skip_after_semicolon();
            return NULL;
        }

        if (!skip_token(SEMICOLON)) {
            skip_after_semicolon();
            return NULL;
        }

        return ::std::unique_ptr<AST::UseDeclaration>(new AST::UseDeclaration(
          ::std::move(use_tree), ::std::move(vis), ::std::move(outer_attrs), locus));
    }

    // Parses a use tree (which can be recursive and is actually a base class).
    ::std::unique_ptr<AST::UseTree> Parser::parse_use_tree() {
        /* potential syntax definitions in attempt to get algorithm:
         *  Glob:
         *      <- SimplePath :: *
         *      <- :: *
         *      <- *
         *  Nested tree thing:
         *      <- SimplePath :: { COMPLICATED_INNER_TREE_THING }
         *      <- :: COMPLICATED_INNER_TREE_THING }
         *      <- { COMPLICATED_INNER_TREE_THING }
         *  Rebind thing:
         *      <- SimplePath as IDENTIFIER
         *      <- SimplePath as _
         *      <- SimplePath
         */

        /* current plan of attack: try to parse SimplePath first - if fails, one of top two
         * then try parse :: - if fails, one of top two. Next is deciding character for top two. */

        // Thus, parsing smaller parts of use tree may require feeding into function via parameters
        // (or could handle all in this single function because other use tree types aren't
        // recognised) as separate in the spec

        // TODO: I think this function is too complex, probably should split it

        location_t locus = lexer.peek_token()->get_locus();

        // bool has_path = false;
        AST::SimplePath path = parse_simple_path();

        if (path.is_empty()) {
            // has no path, so must be glob or nested tree UseTree type

            /* due to implementation issues, parsing simple path removes any trailing scope
             * resolutions (or any, actually, if the use tree has no path given), so we'll just
             * assume that there's one there. */
            // Check anyway, but optional.
            if (lexer.peek_token()->get_id() == SCOPE_RESOLUTION) {
                lexer.skip_token();
            }
            /* Note that this implementation issue also makes it impossible to determine at the
             * moment whether the tree has GLOBAL or NO_PATH path type. */

            const_TokenPtr t = lexer.peek_token();
            switch (t->get_id()) {
                case ASTERISK:
                    // glob UseTree type
                    lexer.skip_token();

                    // TODO: find way to determine whether GLOBAL or NO_PATH path type - placeholder
                    return ::std::unique_ptr<AST::UseTreeGlob>(new AST::UseTreeGlob(
                      AST::UseTreeGlob::NO_PATH, AST::SimplePath::create_empty(), locus));
                case LEFT_CURLY: {
                    // nested tree UseTree type
                    lexer.skip_token();

                    ::std::vector< ::std::unique_ptr<AST::UseTree> > use_trees;

                    const_TokenPtr t = lexer.peek_token();
                    while (t->get_id() != RIGHT_CURLY) {
                        ::std::unique_ptr<AST::UseTree> use_tree = parse_use_tree();
                        if (use_tree == NULL) {
                            break;
                        }

                        use_trees.push_back(::std::move(use_tree));

                        if (lexer.peek_token()->get_id() != COMMA) {
                            break;
                        }
                        lexer.skip_token();

                        t = lexer.peek_token();
                    }

                    // skip end curly delimiter
                    if (!skip_token(RIGHT_CURLY)) {
                        // skip after somewhere?
                        return NULL;
                    }

                    // TODO: find way to determine whether GLOBAL or NO_PATH path type - placeholder
                    return ::std::unique_ptr<AST::UseTreeList>(
                      new AST::UseTreeList(AST::UseTreeList::NO_PATH, AST::SimplePath::create_empty(),
                        ::std::move(use_trees), locus));
                }
                case AS:
                    // this is not allowed
                    error_at(t->get_locus(),
                      "use declaration with rebind 'as' requires a valid simple path - none found.");
                    skip_after_semicolon();
                    return NULL;
                default:
                    error_at(t->get_locus(),
                      "unexpected token '%s' in use tree with no valid simple path (i.e. list or "
                      "glob use tree)",
                      t->get_token_description());
                    skip_after_semicolon();
                    return NULL;
            }
        } else {
            /* Due to aforementioned implementation issues, the trailing :: token is consumed by the
             * path, so it can not be used as a disambiguator. */

            const_TokenPtr t = lexer.peek_token();
            switch (t->get_id()) {
                case ASTERISK:
                    // glob UseTree type
                    lexer.skip_token();

                    return ::std::unique_ptr<AST::UseTreeGlob>(new AST::UseTreeGlob(
                      AST::UseTreeGlob::PATH_PREFIXED, ::std::move(path), locus));
                case LEFT_CURLY: {
                    // nested tree UseTree type
                    lexer.skip_token();

                    ::std::vector< ::std::unique_ptr<AST::UseTree> > use_trees;

                    // TODO: think of better control structure
                    const_TokenPtr t = lexer.peek_token();
                    while (t->get_id() != RIGHT_CURLY) {
                        ::std::unique_ptr<AST::UseTree> use_tree = parse_use_tree();
                        if (use_tree == NULL) {
                            break;
                        }

                        use_trees.push_back(::std::move(use_tree));

                        if (lexer.peek_token()->get_id() != COMMA) {
                            break;
                        }
                        lexer.skip_token();

                        t = lexer.peek_token();
                    }

                    // skip end curly delimiter
                    if (!skip_token(RIGHT_CURLY)) {
                        // skip after somewhere?
                        return NULL;
                    }

                    return ::std::unique_ptr<AST::UseTreeList>(
                      new AST::UseTreeList(AST::UseTreeList::PATH_PREFIXED, ::std::move(path),
                        std::move(use_trees), locus));
                }
                case AS: {
                    // rebind UseTree type
                    lexer.skip_token();

                    const_TokenPtr t = lexer.peek_token();
                    switch (t->get_id()) {
                        case IDENTIFIER:
                            // skip lexer token
                            lexer.skip_token();

                            return ::std::unique_ptr<AST::UseTreeRebind>(
                              new AST::UseTreeRebind(AST::UseTreeRebind::IDENTIFIER,
                                ::std::move(path), locus, t->get_str()));
                        case UNDERSCORE:
                            // skip lexer token
                            lexer.skip_token();

                            return ::std::unique_ptr<AST::UseTreeRebind>(
                              new AST::UseTreeRebind(AST::UseTreeRebind::WILDCARD, ::std::move(path),
                                locus, ::std::string("_")));
                        default:
                            error_at(t->get_locus(),
                              "unexpected token '%s' in use tree with as clause - expected "
                              "identifier or '_'",
                              t->get_token_description());
                            skip_after_semicolon();
                            return NULL;
                    }
                }
                case SEMICOLON:
                    // rebind UseTree type without rebinding - path only

                    // don't skip semicolon - handled in parse_use_tree
                    // lexer.skip_token();

                    return ::std::unique_ptr<AST::UseTreeRebind>(
                      new AST::UseTreeRebind(AST::UseTreeRebind::NONE, ::std::move(path), locus));
                case COMMA:
                case RIGHT_CURLY:
                    // this may occur in recursive calls - assume it is ok and ignore it
                    return ::std::unique_ptr<AST::UseTreeRebind>(
                      new AST::UseTreeRebind(AST::UseTreeRebind::NONE, ::std::move(path), locus));
                default:
                    error_at(t->get_locus(), "unexpected token '%s' in use tree with valid path",
                      t->get_token_description());
                    // skip_after_semicolon();
                    return NULL;
            }
        }
    }

    // Parses a function (not a method).
    ::std::unique_ptr<AST::Function> Parser::parse_function(
      AST::Visibility vis, ::std::vector<AST::Attribute> outer_attrs) {
        location_t locus = lexer.peek_token()->get_locus();
        // Get qualifiers for function if they exist
        AST::FunctionQualifiers qualifiers = parse_function_qualifiers();

        skip_token(FN_TOK);

        // Save function name token
        const_TokenPtr function_name_tok = expect_token(IDENTIFIER);
        if (function_name_tok == NULL) {
            skip_after_next_block();
            return NULL;
        }
        Identifier function_name = function_name_tok->get_str();

        // parse generic params - if exist
        ::std::vector< ::std::unique_ptr<AST::GenericParam> > generic_params
          = parse_generic_params_in_angles();

        if (!skip_token(LEFT_PAREN)) {
            error_at(lexer.peek_token()->get_locus(),
              "function declaration missing opening parentheses before parameter list");
            skip_after_next_block();
            return NULL;
        }

        // parse function parameters (only if next token isn't right paren)
        ::std::vector<AST::FunctionParam> function_params;
        if (lexer.peek_token()->get_id() != RIGHT_PAREN) {
            function_params = parse_function_params();
        }

        if (!skip_token(RIGHT_PAREN)) {
            error_at(lexer.peek_token()->get_locus(),
              "function declaration missing closing parentheses after parameter list");
            skip_after_next_block();
            return NULL;
        }

        // parse function return type - if exists
        ::std::unique_ptr<AST::Type> return_type = parse_function_return_type();

        // parse where clause - if exists
        AST::WhereClause where_clause = parse_where_clause();

        // parse block expression
        ::std::unique_ptr<AST::BlockExpr> block_expr = parse_block_expr();

        return ::std::unique_ptr<AST::Function>(new AST::Function(::std::move(function_name),
          ::std::move(qualifiers), ::std::move(generic_params), ::std::move(function_params),
          ::std::move(return_type), ::std::move(where_clause), ::std::move(block_expr),
          ::std::move(vis), ::std::move(outer_attrs), locus));
    }

    // Parses function or method qualifiers (i.e. const, unsafe, and extern).
    AST::FunctionQualifiers Parser::parse_function_qualifiers() {
        AST::FunctionQualifiers::AsyncConstStatus const_status = AST::FunctionQualifiers::NONE;
        // bool has_const = false;
        bool has_unsafe = false;
        bool has_extern = false;
        ::std::string abi;

        // Check in order of const, unsafe, then extern
        const_TokenPtr t = lexer.peek_token();
        switch (t->get_id()) {
            case CONST:
                lexer.skip_token();
                const_status = AST::FunctionQualifiers::CONST;
                break;
            case ASYNC:
                lexer.skip_token();
                const_status = AST::FunctionQualifiers::ASYNC;
                break;
            default:
                // const status is still none
                break;
        }

        if (lexer.peek_token()->get_id() == UNSAFE) {
            lexer.skip_token();
            has_unsafe = true;
        }

        if (lexer.peek_token()->get_id() == EXTERN_TOK) {
            lexer.skip_token();
            has_extern = true;

            // detect optional abi name
            const_TokenPtr next_tok = lexer.peek_token();
            if (next_tok->get_id() == STRING_LITERAL) {
                lexer.skip_token();
                abi = next_tok->get_str();
            }
        }

        return AST::FunctionQualifiers(const_status, has_unsafe, has_extern, ::std::move(abi));
    }

    // Parses generic (lifetime or type) params inside angle brackets (optional).
    ::std::vector< ::std::unique_ptr<AST::GenericParam> > Parser::parse_generic_params_in_angles() {
        if (lexer.peek_token()->get_id() != LEFT_ANGLE) {
            // seems to be no generic params, so exit with empty vector
            return ::std::vector< ::std::unique_ptr<AST::GenericParam> >();
        }
        lexer.skip_token();

        // DEBUG:
        fprintf(stderr, "skipped left angle in generic param\n");

        ::std::vector< ::std::unique_ptr<AST::GenericParam> > generic_params = parse_generic_params();

        // DEBUG:
        fprintf(stderr, "finished parsing actual generic params (i.e. inside angles)\n");

        if (!skip_generics_right_angle()) {
            // DEBUG
            fprintf(stderr, "failed to skip generics right angle - returning empty generic params\n");

            return ::std::vector< ::std::unique_ptr<AST::GenericParam> >();
        }

        return generic_params;
    }

    /* Parse generic (lifetime or type) params NOT INSIDE ANGLE BRACKETS!!! Almost always
     * parse_generic_params_in_angles is what is wanted. */
    ::std::vector< ::std::unique_ptr<AST::GenericParam> > Parser::parse_generic_params() {
        ::std::vector< ::std::unique_ptr<AST::GenericParam> > generic_params;

        // can't parse lifetime and type params separately due to lookahead issues
        // thus, parse them all here

        // DEBUG
        fprintf(stderr, "starting to parse generic params (inside angle brackets)\n");

        // HACK: used to retain attribute data if a lifetime param is tentatively parsed but it turns
        // out to be type param
        AST::Attribute parsed_outer_attr = AST::Attribute::create_empty();

        // HACK: generic params always in angle brackets with current syntax, so have that as end char
        const_TokenPtr t = lexer.peek_token();
        // parse lifetime params
        while (!is_right_angle_tok(t->get_id())) {
            // HACK: reimpl of lifetime param parsing
            AST::Attribute outer_attr = parse_outer_attribute();

            // move attribute outward if type param
            if (lexer.peek_token()->get_id() != LIFETIME) {
                parsed_outer_attr = ::std::move(outer_attr);

                // DEBUG
                fprintf(stderr, "broke from parsing lifetime params as next token isn't lifetime - "
                                "saved attribute\n");

                break;
            }

            location_t locus = lexer.peek_token()->get_locus();
            AST::Lifetime lifetime = parse_lifetime();

            // DEBUG
            fprintf(stderr, "parsed lifetime in lifetime params\n");

            // parse optional bounds
            ::std::vector<AST::Lifetime> lifetime_bounds;
            if (lexer.peek_token()->get_id() == COLON) {
                lexer.skip_token();
                // parse required bounds
                lifetime_bounds = parse_lifetime_bounds();
            }

            ::std::unique_ptr<AST::LifetimeParam> param(new AST::LifetimeParam(
              ::std::move(lifetime), locus, ::std::move(lifetime_bounds), ::std::move(outer_attr)));
            generic_params.push_back(::std::move(param));

            if (lexer.peek_token()->get_id() != COMMA) {
                break;
            }
            lexer.skip_token();

            t = lexer.peek_token();
        }

        // parse type params (reimpl required for first one but not others)
        if (!is_right_angle_tok(lexer.peek_token()->get_id()) && !parsed_outer_attr.is_empty()) {
            // DEBUG
            fprintf(stderr, "as parsed outer attr isn't empty, started parsing type param reimpl\n");

            // reimpl as type param definitely exists
            const_TokenPtr ident_tok = expect_token(IDENTIFIER);
            if (ident_tok == NULL) {
                error_at(lexer.peek_token()->get_locus(),
                  "failed to parse identifier in type param in generic params");
                return ::std::vector< ::std::unique_ptr<AST::GenericParam> >();
            }
            Identifier ident = ident_tok->get_str();

            // parse optional bounds
            ::std::vector< ::std::unique_ptr<AST::TypeParamBound> > type_param_bounds;
            if (lexer.peek_token()->get_id() == COLON) {
                lexer.skip_token();

                // parse optional type param bounds
                type_param_bounds = parse_type_param_bounds();
            }

            // parse optional type
            ::std::unique_ptr<AST::Type> type = NULL;
            if (lexer.peek_token()->get_id() == EQUAL) {
                lexer.skip_token();

                // parse required type
                type = parse_type();
                if (type == NULL) {
                    error_at(lexer.peek_token()->get_id(),
                      "failed to parse type in type param in generic params");
                    return ::std::vector< ::std::unique_ptr<AST::GenericParam> >();
                }
            }

            ::std::unique_ptr<AST::TypeParam> param(
              new AST::TypeParam(::std::move(ident), ident_tok->get_locus(),
                ::std::move(type_param_bounds), ::std::move(type), ::std::move(parsed_outer_attr)));
            generic_params.push_back(::std::move(param));

            // handle comma
            if (lexer.peek_token()->get_id() == COMMA) {
                lexer.skip_token();
            }
        }

        // DEBUG
        fprintf(stderr, "about to start parsing normally-parsed type params in generic params\n");

        // parse rest of type params - reimpl due to right angle tokens
        t = lexer.peek_token();
        while (!is_right_angle_tok(t->get_id())) {
            ::std::unique_ptr<AST::TypeParam> type_param = parse_type_param();

            if (type_param == NULL) {
                error_at(
                  lexer.peek_token()->get_locus(), "failed to parse type param in generic params");
                return ::std::vector< ::std::unique_ptr<AST::GenericParam> >();
            }

            // DEBUG
            fprintf(stderr, "successfully parsed type param\n");

            generic_params.push_back(::std::move(type_param));

            if (lexer.peek_token()->get_id() != COMMA) {
                break;
            }
            // skip commas, including trailing commas
            lexer.skip_token();

            t = lexer.peek_token();
        }

        // old code
        /*
        // parse lifetime params (optional), allowed to end with a trailing comma
        ::std::vector< ::std::unique_ptr<AST::LifetimeParam> > lifetime_params
          = parse_lifetime_params();
        if (!lifetime_params.empty()) {
            // C++11 code:
            generic_params.insert(generic_params.end(),
              ::std::make_move_iterator(lifetime_params.begin()),
              ::std::make_move_iterator(lifetime_params.end()));
        }

        // parse type params (optional)
        ::std::vector< ::std::unique_ptr<AST::TypeParam> > type_params = parse_type_params();
        if (!type_params.empty()) {
            // C++11 code:
            generic_params.insert(generic_params.end(),
              ::std::make_move_iterator(type_params.begin()),
              ::std::make_move_iterator(type_params.end()));
        }*/

        return generic_params;
    }

    // Parses lifetime generic parameters (pointers). Will also consume any trailing comma.
    ::std::vector< ::std::unique_ptr<AST::LifetimeParam> > Parser::parse_lifetime_params() {
        ::std::vector< ::std::unique_ptr<AST::LifetimeParam> > lifetime_params;

        // TODO: think of better control structure than infinite loop with break on failure?
        while (true) {
            AST::LifetimeParam lifetime_param = parse_lifetime_param();

            if (lifetime_param.is_error()) {
                // break if fails to parse
                break;
            }

            lifetime_params.push_back(
              ::std::unique_ptr<AST::LifetimeParam>(new AST::LifetimeParam(lifetime_param)));

            if (lexer.peek_token()->get_id() != COMMA) {
                break;
            }
            // skip commas, including trailing commas
            lexer.skip_token();
        }

        return lifetime_params;
    }

    /* Parses lifetime generic parameters (objects). Will also consume any trailing comma.
     * TODO: is this best solution? implements most of the same algorithm. */
    ::std::vector<AST::LifetimeParam> Parser::parse_lifetime_params_objs() {
        ::std::vector<AST::LifetimeParam> lifetime_params;

        // TODO: think of better control structure than infinite loop with break on failure?
        while (true) {
            AST::LifetimeParam lifetime_param = parse_lifetime_param();

            if (lifetime_param.is_error()) {
                // break if fails to parse
                break;
            }

            lifetime_params.push_back(lifetime_param);

            if (lexer.peek_token()->get_id() != COMMA) {
                break;
            }
            // skip commas, including trailing commas
            lexer.skip_token();
        }

        return lifetime_params;
    }

    /* Parses a single lifetime generic parameter (not including comma). */
    AST::LifetimeParam Parser::parse_lifetime_param() {
        // parse outer attribute, which is optional and may not exist
        AST::Attribute outer_attr = parse_outer_attribute();

        // save lifetime token - required
        const_TokenPtr lifetime_tok = lexer.peek_token();
        if (lifetime_tok->get_id() != LIFETIME) {
            // if lifetime is missing, must not be a lifetime param, so return null
            return AST::LifetimeParam::create_error();
        }
        // TODO: does this always create a named lifetime? or can a different type be made?
        AST::Lifetime lifetime(
          AST::Lifetime::NAMED, lifetime_tok->get_str(), lifetime_tok->get_locus());

        // parse lifetime bounds, if it exists
        ::std::vector<AST::Lifetime> lifetime_bounds;
        if (lexer.peek_token()->get_id() == COLON) {
            // parse lifetime bounds
            lifetime_bounds = parse_lifetime_bounds();
        }

        return AST::LifetimeParam(::std::move(lifetime), lifetime_tok->get_locus(),
          ::std::move(lifetime_bounds), ::std::move(outer_attr));
    }

    // Parses type generic parameters. Will also consume any trailing comma.
    ::std::vector< ::std::unique_ptr<AST::TypeParam> > Parser::parse_type_params() {
        ::std::vector< ::std::unique_ptr<AST::TypeParam> > type_params;

        // TODO: think of better control structure than infinite loop with break on failure?
        while (true) {
            ::std::unique_ptr<AST::TypeParam> type_param = parse_type_param();

            if (type_param == NULL) {
                // break if fails to parse
                break;
            }

            type_params.push_back(::std::move(type_param));

            if (lexer.peek_token()->get_id() != COMMA) {
                break;
            }
            // skip commas, including trailing commas
            lexer.skip_token();
        }

        return type_params;
        // TODO: this shares most code with parse_lifetime_params - good place to use template?
    }

    // Parses a single type (generic) parameter, not including commas. May change to return value.
    ::std::unique_ptr<AST::TypeParam> Parser::parse_type_param() {
        // parse outer attribute, which is optional and may not exist
        AST::Attribute outer_attr = parse_outer_attribute();

        const_TokenPtr identifier_tok = lexer.peek_token();
        if (identifier_tok->get_id() != IDENTIFIER) {
            // return null as type param can't exist without this required identifier
            return NULL;
        }
        // TODO: create identifier from identifier token
        Identifier ident = identifier_tok->get_str();
        lexer.skip_token();

        // parse type param bounds (if they exist)
        ::std::vector< ::std::unique_ptr<AST::TypeParamBound> > type_param_bounds;
        if (lexer.peek_token()->get_id() == COLON) {
            lexer.skip_token();

            // parse type param bounds, which may or may not exist
            type_param_bounds = parse_type_param_bounds();
        }

        // parse type (if it exists)
        ::std::unique_ptr<AST::Type> type = NULL;
        if (lexer.peek_token()->get_id() == EQUAL) {
            lexer.skip_token();

            // parse type (now required)
            type = parse_type();
            if (type == NULL) {
                error_at(lexer.peek_token()->get_locus(), "failed to parse type in type param");
                return NULL;
            }
        }

        return ::std::unique_ptr<AST::TypeParam>(
          new AST::TypeParam(::std::move(ident), identifier_tok->get_locus(),
            ::std::move(type_param_bounds), ::std::move(type), ::std::move(outer_attr)));
    }

    // Parses regular (i.e. non-generic) parameters in functions or methods.
    ::std::vector<AST::FunctionParam> Parser::parse_function_params() {
        ::std::vector<AST::FunctionParam> params;

        // HACK: return early if RIGHT_PAREN is found
        if (lexer.peek_token()->get_id() == RIGHT_PAREN) {
            return params;
        }

        AST::FunctionParam initial_param = parse_function_param();

        // Return empty parameter list if no parameter there
        if (initial_param.is_error()) {
            return params;
        }

        params.push_back(::std::move(initial_param));

        // maybe think of a better control structure here - do-while with an initial error state?
        // basically, loop through parameter list until can't find any more params
        const_TokenPtr t = lexer.peek_token();
        while (t->get_id() == COMMA) {
            // skip comma if applies
            lexer.skip_token();

            /* HACK: break if next token is a right (closing) paren - this is not strictly true via
             * grammar rule but seems to be true in practice (i.e. with trailing comma). */
            if (lexer.peek_token()->get_id() == RIGHT_PAREN) {
                break;
            }

            // now, as right paren would break, function param is required
            AST::FunctionParam param = parse_function_param();
            if (param.is_error()) {
                error_at(lexer.peek_token()->get_locus(),
                  "failed to parse function param (in function params)");
                // skip somewhere?
                return ::std::vector<AST::FunctionParam>();
            }

            params.push_back(::std::move(param));

            t = lexer.peek_token();
        }

        return params;
    }

    /* Parses a single regular (i.e. non-generic) parameter in a function or method, i.e. the
     * "name: type" bit. Also handles it not existing. */
    AST::FunctionParam Parser::parse_function_param() {
        location_t locus = lexer.peek_token()->get_locus();
        ::std::unique_ptr<AST::Pattern> param_pattern = parse_pattern();

        // create error function param if it doesn't exist
        if (param_pattern == NULL) {
            // skip after something
            return AST::FunctionParam::create_error();
        }

        if (!skip_token(COLON)) {
            // skip after something
            return AST::FunctionParam::create_error();
        }

        ::std::unique_ptr<AST::Type> param_type = parse_type();
        if (param_type == NULL) {
            // skip?
            return AST::FunctionParam::create_error();
        }

        return AST::FunctionParam(::std::move(param_pattern), ::std::move(param_type), locus);
    }

    /* Parses a function or method return type syntactical construction. Also handles a function
     * return type not existing. */
    ::std::unique_ptr<AST::Type> Parser::parse_function_return_type() {
        if (lexer.peek_token()->get_id() != RETURN_TYPE) {
            return NULL;
        }
        // skip return type, as it now obviously exists
        lexer.skip_token();

        ::std::unique_ptr<AST::Type> type = parse_type();

        return type;
    }

    /* Parses a "where clause" (in a function, struct, method, etc.). Also handles a where clause
     * not existing, in which it will return WhereClause::create_empty(), which can be checked via
     * WhereClause::is_empty(). */
    AST::WhereClause Parser::parse_where_clause() {
        const_TokenPtr where_tok = lexer.peek_token();
        if (where_tok->get_id() != WHERE) {
            // where clause doesn't exist, so create empty one
            return AST::WhereClause::create_empty();
        }

        lexer.skip_token();

        // parse where clause items - this is not a separate rule in the reference so won't be here
        ::std::vector< ::std::unique_ptr<AST::WhereClauseItem> > where_clause_items;

        // HACK: where clauses end with a right curly or semicolon or equals in all uses currently
        const_TokenPtr t = lexer.peek_token();
        while (t->get_id() != LEFT_CURLY && t->get_id() != SEMICOLON && t->get_id() != EQUAL) {
            ::std::unique_ptr<AST::WhereClauseItem> where_clause_item = parse_where_clause_item();

            if (where_clause_item == NULL) {
                error_at(t->get_locus(), "failed to parse where clause item");
                return AST::WhereClause::create_empty();
            }

            where_clause_items.push_back(::std::move(where_clause_item));

            // also skip comma if it exists
            if (lexer.peek_token()->get_id() != COMMA) {
                break;
            }
            lexer.skip_token();

            t = lexer.peek_token();
        }

        return AST::WhereClause(::std::move(where_clause_items));
    }

    // Parses a where clause item (lifetime or type bound). Does not parse any commas.
    ::std::unique_ptr<AST::WhereClauseItem> Parser::parse_where_clause_item() {
        // shitty cheat way of determining lifetime or type bound - test for lifetime
        const_TokenPtr t = lexer.peek_token();

        if (t->get_id() == LIFETIME) {
            return parse_lifetime_where_clause_item();
        } else {
            return parse_type_bound_where_clause_item();
        }
    }

    // Parses a lifetime where clause item.
    ::std::unique_ptr<AST::LifetimeWhereClauseItem> Parser::parse_lifetime_where_clause_item() {
        AST::Lifetime lifetime = parse_lifetime();
        if (lifetime.is_error()) {
            // TODO: error here?
            return NULL;
        }

        if (!skip_token(COLON)) {
            // TODO: skip after somewhere
            return NULL;
        }

        ::std::vector<AST::Lifetime> lifetime_bounds = parse_lifetime_bounds();

        return ::std::unique_ptr<AST::LifetimeWhereClauseItem>(
          new AST::LifetimeWhereClauseItem(::std::move(lifetime), ::std::move(lifetime_bounds)));
    }

    // Parses a type bound where clause item.
    ::std::unique_ptr<AST::TypeBoundWhereClauseItem> Parser::parse_type_bound_where_clause_item() {
        // parse for lifetimes, if it exists
        ::std::vector<AST::LifetimeParam> for_lifetimes;
        if (lexer.peek_token()->get_id() == FOR) {
            for_lifetimes = parse_for_lifetimes();
        }

        ::std::unique_ptr<AST::Type> type = parse_type();
        if (type == NULL) {
            return NULL;
        }

        if (!skip_token(COLON)) {
            // TODO: skip after somewhere
            return NULL;
        }

        // parse type param bounds if they exist
        ::std::vector< ::std::unique_ptr<AST::TypeParamBound> > type_param_bounds
          = parse_type_param_bounds();

        return ::std::unique_ptr<AST::TypeBoundWhereClauseItem>(new AST::TypeBoundWhereClauseItem(
          ::std::move(for_lifetimes), ::std::move(type), ::std::move(type_param_bounds)));
    }

    // Parses a for lifetimes clause, including the for keyword and angle brackets.
    ::std::vector<AST::LifetimeParam> Parser::parse_for_lifetimes() {
        ::std::vector<AST::LifetimeParam> params;

        if (!skip_token(FOR)) {
            // skip after somewhere?
            return params;
        }

        if (!skip_token(LEFT_ANGLE)) {
            // skip after somewhere?
            return params;
        }

        params = parse_lifetime_params_objs();

        if (!skip_generics_right_angle()) {
            // skip after somewhere?
            return params;
        }

        return params;
    }

    // Parses type parameter bounds in where clause or generic arguments.
    ::std::vector< ::std::unique_ptr<AST::TypeParamBound> > Parser::parse_type_param_bounds() {
        ::std::vector< ::std::unique_ptr<AST::TypeParamBound> > type_param_bounds;

        ::std::unique_ptr<AST::TypeParamBound> initial_bound = parse_type_param_bound();

        // quick exit if null
        if (initial_bound == NULL) {
            // error? type param bounds must have at least one term, but are bounds optional?
            return type_param_bounds;
        }

        type_param_bounds.push_back(::std::move(initial_bound));

        // TODO think of better control structure than infinite loop
        while (true) {
            // Quick exit for no more bounds
            if (lexer.peek_token()->get_id() != PLUS) {
                return type_param_bounds;
            }
            lexer.skip_token();

            ::std::unique_ptr<AST::TypeParamBound> bound = parse_type_param_bound();
            if (bound == NULL) {
                // not an error: bound is allowed to be null as trailing plus is allowed
                return type_param_bounds;
            }

            type_param_bounds.push_back(::std::move(bound));
        }

        return type_param_bounds;
    }

    /* Parses a single type parameter bound in a where clause or generic argument. Does not parse
     * the '+' between arguments. */
    ::std::unique_ptr<AST::TypeParamBound> Parser::parse_type_param_bound() {
        // shitty cheat way of determining lifetime or trait bound - test for lifetime
        const_TokenPtr t = lexer.peek_token();
        switch (t->get_id()) {
            case LIFETIME:
                return ::std::unique_ptr<AST::Lifetime>(new AST::Lifetime(parse_lifetime()));
            case LEFT_PAREN:
            case QUESTION_MARK:
            case FOR:
            case IDENTIFIER:
            case SUPER:
            case SELF:
            case SELF_ALIAS:
            case CRATE:
            case DOLLAR_SIGN:
                return parse_trait_bound();
            default:
                // don't error - assume this is fine TODO
                return NULL;
        }
    }

    // Parses a trait bound type param bound.
    ::std::unique_ptr<AST::TraitBound> Parser::parse_trait_bound() {
        bool has_parens = false;
        bool has_question_mark = false;

        location_t locus = lexer.peek_token()->get_locus();

        // handle trait bound being in parentheses
        if (lexer.peek_token()->get_id() == LEFT_PAREN) {
            has_parens = true;
            lexer.skip_token();
        }

        // handle having question mark (optional)
        if (lexer.peek_token()->get_id() == QUESTION_MARK) {
            has_question_mark = true;
            lexer.skip_token();
        }

        // parse for lifetimes, if it exists (although empty for lifetimes is ok to handle this)
        ::std::vector<AST::LifetimeParam> for_lifetimes;
        if (lexer.peek_token()->get_id() == FOR) {
            for_lifetimes = parse_for_lifetimes();
        }

        // handle TypePath
        AST::TypePath type_path = parse_type_path();

        // handle closing parentheses
        if (has_parens) {
            if (!skip_token(RIGHT_PAREN)) {
                return NULL;
            }
        }

        return ::std::unique_ptr<AST::TraitBound>(new AST::TraitBound(
          ::std::move(type_path), locus, has_parens, has_question_mark, ::std::move(for_lifetimes)));
    }

    // Parses lifetime bounds.
    ::std::vector<AST::Lifetime> Parser::parse_lifetime_bounds() {
        ::std::vector<AST::Lifetime> lifetime_bounds;

        // TODO: think of better control structure
        while (true) {
            AST::Lifetime lifetime = parse_lifetime();

            // quick exit for parsing failure
            if (lifetime.is_error()) {
                return lifetime_bounds;
            }

            lifetime_bounds.push_back(::std::move(lifetime));

            // plus is maybe required - spec defines it poorly, so assuming not required
            if (lexer.peek_token()->get_id() != PLUS) {
                return lifetime_bounds;
            }

            lexer.skip_token();
        }

        return lifetime_bounds;
    }

    // Parses a lifetime token (named, 'static, or '_). Also handles lifetime not existing.
    AST::Lifetime Parser::parse_lifetime() {
        const_TokenPtr lifetime_tok = lexer.peek_token();
        location_t locus = lifetime_tok->get_locus();
        // create error lifetime if doesn't exist
        if (lifetime_tok->get_id() != LIFETIME) {
            return AST::Lifetime::error();
        }
        lexer.skip_token();

        ::std::string lifetime_ident = lifetime_tok->get_str();

        if (lifetime_ident == "'static") {
            return AST::Lifetime(AST::Lifetime::STATIC, "", locus);
        } else if (lifetime_ident == "'_") {
            return AST::Lifetime(AST::Lifetime::WILDCARD, "", locus);
        } else {
            return AST::Lifetime(AST::Lifetime::NAMED, ::std::move(lifetime_ident), locus);
        }
    }

    // Parses a "type alias" (typedef) item.
    ::std::unique_ptr<AST::TypeAlias> Parser::parse_type_alias(
      AST::Visibility vis, ::std::vector<AST::Attribute> outer_attrs) {
        location_t locus = lexer.peek_token()->get_locus();
        skip_token(TYPE);

        // TODO: use this token for identifier when finished that
        const_TokenPtr alias_name_tok = expect_token(IDENTIFIER);
        if (alias_name_tok == NULL) {
            error_at(lexer.peek_token()->get_locus(), "could not parse identifier in type alias");
            skip_after_semicolon();
            return NULL;
        }
        Identifier alias_name = alias_name_tok->get_str();

        // parse generic params, which may not exist
        ::std::vector< ::std::unique_ptr<AST::GenericParam> > generic_params
          = parse_generic_params_in_angles();

        // parse where clause, which may not exist
        AST::WhereClause where_clause = parse_where_clause();

        if (!skip_token(EQUAL)) {
            skip_after_semicolon();
            return NULL;
        }

        ::std::unique_ptr<AST::Type> type_to_alias = parse_type();

        if (!skip_token(SEMICOLON)) {
            // should be skipping past this, not the next line
            return NULL;
        }

        return ::std::unique_ptr<AST::TypeAlias>(new AST::TypeAlias(::std::move(alias_name),
          ::std::move(generic_params), ::std::move(where_clause), ::std::move(type_to_alias),
          ::std::move(vis), ::std::move(outer_attrs), locus));
    }

    // Parse a struct item AST node.
    ::std::unique_ptr<AST::Struct> Parser::parse_struct(
      AST::Visibility vis, ::std::vector<AST::Attribute> outer_attrs) {
        /* TODO: determine best way to parse the proper struct vs tuple struct - share most of
         * initial constructs so lookahead might be impossible, and if not probably too expensive.
         * Best way is probably unified parsing for the initial parts and then pass them in as params
         * to more derived functions. Alternatively, just parse everything in this one function -
         * do this if function not too long. */

        /* Proper struct <- 'struct' IDENTIFIER generic_params? where_clause? ( '{' struct_fields? '}'
         * | ';' ) */
        /* Tuple struct <- 'struct' IDENTIFIER generic_params? '(' tuple_fields? ')'
         * where_clause? ';' */
        location_t locus = lexer.peek_token()->get_locus();
        skip_token(STRUCT_TOK);

        // parse struct name
        const_TokenPtr name_tok = expect_token(IDENTIFIER);
        if (name_tok == NULL) {
            error_at(
              lexer.peek_token()->get_locus(), "could not parse struct or tuple struct identifier");
            // skip after somewhere?
            return NULL;
        }
        Identifier struct_name = name_tok->get_str();

        // parse generic params, which may or may not exist
        ::std::vector< ::std::unique_ptr<AST::GenericParam> > generic_params
          = parse_generic_params_in_angles();

        // branch on next token - determines whether proper struct or tuple struct
        if (lexer.peek_token()->get_id() == LEFT_PAREN) {
            // tuple struct

            // skip left parenthesis
            lexer.skip_token();

            // parse tuple fields
            ::std::vector<AST::TupleField> tuple_fields = parse_tuple_fields();

            // tuple parameters must have closing parenthesis
            if (!skip_token(RIGHT_PAREN)) {
                skip_after_semicolon();
                return NULL;
            }

            // parse where clause, which is optional
            AST::WhereClause where_clause = parse_where_clause();

            if (!skip_token(SEMICOLON)) {
                // can't skip after semicolon because it's meant to be here
                return NULL;
            }

            return ::std::unique_ptr<AST::TupleStruct>(new AST::TupleStruct(::std::move(tuple_fields),
              ::std::move(struct_name), ::std::move(generic_params), ::std::move(where_clause),
              ::std::move(vis), ::std::move(outer_attrs), locus));
        }

        // assume it is a proper struct being parsed and continue outside of switch - label only here
        // to suppress warning

        // parse where clause, which is optional
        AST::WhereClause where_clause = parse_where_clause();

        // branch on next token - determines whether struct is a unit struct
        const_TokenPtr t = lexer.peek_token();
        switch (t->get_id()) {
            case LEFT_CURLY: {
                // struct with body

                // skip curly bracket
                lexer.skip_token();

                // parse struct fields, if any
                ::std::vector<AST::StructField> struct_fields = parse_struct_fields();

                if (!skip_token(RIGHT_CURLY)) {
                    // skip somewhere?
                    return NULL;
                }

                return ::std::unique_ptr<AST::StructStruct>(
                  new AST::StructStruct(::std::move(struct_fields), ::std::move(struct_name),
                    ::std::move(generic_params), ::std::move(where_clause), false, ::std::move(vis),
                    ::std::move(outer_attrs), locus));
            }
            case SEMICOLON:
                // unit struct declaration

                lexer.skip_token();

                return ::std::unique_ptr<AST::StructStruct>(
                  new AST::StructStruct(::std::move(struct_name), ::std::move(generic_params),
                    ::std::move(where_clause), ::std::move(vis), ::std::move(outer_attrs), locus));
            default:
                error_at(t->get_locus(), "unexpected token '%s' in struct declaration",
                  t->get_token_description());
                // skip somewhere?
                return NULL;
        }
    }

    // Parses struct fields in struct declarations.
    ::std::vector<AST::StructField> Parser::parse_struct_fields() {
        ::std::vector<AST::StructField> fields;

        AST::StructField initial_field = parse_struct_field();

        // Return empty field list if no field there
        if (initial_field.is_error()) {
            return fields;
        }

        fields.push_back(::std::move(initial_field));

        // maybe think of a better control structure here - do-while with an initial error state?
        // basically, loop through field list until can't find any more params
        while (true) {
            if (lexer.peek_token()->get_id() != COMMA) {
                break;
            }

            // skip comma if applies
            lexer.skip_token();

            AST::StructField field = parse_struct_field();

            if (!field.is_error()) {
                fields.push_back(::std::move(field));
            } else {
                // this would occur with a trailing comma, which is allowed
                break;
            }
        }

        return fields;

        // TODO: this shares basically all code with function params and tuple fields - templates?
    }

    // Parses a single struct field (in a struct definition). Does not parse commas.
    AST::StructField Parser::parse_struct_field() {
        // parse outer attributes, if they exist
        ::std::vector<AST::Attribute> outer_attrs = parse_outer_attributes();

        // parse visibility, if it exists
        AST::Visibility vis = parse_visibility();

        // parse field name
        const_TokenPtr field_name_tok = lexer.peek_token();
        if (field_name_tok->get_id() != IDENTIFIER) {
            // if not identifier, assumes there is no struct field and exits - not necessarily error
            return AST::StructField::create_error();
        }
        Identifier field_name = field_name_tok->get_str();
        lexer.skip_token();

        if (!skip_token(COLON)) {
            // skip after somewhere?
            return AST::StructField::create_error();
        }

        // parse field type - this is required
        ::std::unique_ptr<AST::Type> field_type = parse_type();
        if (field_type == NULL) {
            error_at(
              lexer.peek_token()->get_locus(), "could not parse type in struct field definition");
            // skip after somewhere
            return AST::StructField::create_error();
        }

        return AST::StructField(::std::move(field_name), ::std::move(field_type), ::std::move(vis),
          ::std::move(outer_attrs));
    }

    // Parses tuple fields in tuple/tuple struct declarations.
    ::std::vector<AST::TupleField> Parser::parse_tuple_fields() {
        ::std::vector<AST::TupleField> fields;

        AST::TupleField initial_field = parse_tuple_field();

        // Return empty field list if no field there
        if (initial_field.is_error()) {
            return fields;
        }

        fields.push_back(::std::move(initial_field));

        // maybe think of a better control structure here - do-while with an initial error state?
        // basically, loop through field list until can't find any more params
        // HACK: all current syntax uses of tuple fields have them ending with a right paren token
        const_TokenPtr t = lexer.peek_token();
        while (t->get_id() == COMMA) {
            // skip comma if applies - e.g. trailing comma
            lexer.skip_token();

            // break out due to right paren if it exists
            if (lexer.peek_token()->get_id() == RIGHT_PAREN) {
                break;
            }

            AST::TupleField field = parse_tuple_field();
            if (field.is_error()) {
                error_at(
                  lexer.peek_token()->get_locus(), "failed to parse tuple field in tuple fields");
                return ::std::vector<AST::TupleField>();
            }

            fields.push_back(::std::move(field));

            t = lexer.peek_token();
        }

        return fields;

        // TODO: this shares basically all code with function params and struct fields - templates?
    }

    // Parses a single tuple struct field in a tuple struct definition. Does not parse commas.
    AST::TupleField Parser::parse_tuple_field() {
        // parse outer attributes if they exist
        ::std::vector<AST::Attribute> outer_attrs = parse_outer_attributes();

        // parse visibility if it exists
        AST::Visibility vis = parse_visibility();

        // parse type, which is required
        ::std::unique_ptr<AST::Type> field_type = parse_type();
        if (field_type == NULL) {
            // error if null
            error_at(lexer.peek_token()->get_locus(), "could not parse type in tuple struct field");
            // skip after something
            return AST::TupleField::create_error();
        }

        return AST::TupleField(::std::move(field_type), ::std::move(vis), ::std::move(outer_attrs));
    }

    // Parses a Rust "enum" tagged union item definition.
    ::std::unique_ptr<AST::Enum> Parser::parse_enum(
      AST::Visibility vis, ::std::vector<AST::Attribute> outer_attrs) {
        location_t locus = lexer.peek_token()->get_locus();
        skip_token(ENUM_TOK);

        // parse enum name
        const_TokenPtr enum_name_tok = expect_token(IDENTIFIER);
        Identifier enum_name = enum_name_tok->get_str();

        // parse generic params (of enum container, not enum variants) if they exist
        ::std::vector< ::std::unique_ptr<AST::GenericParam> > generic_params
          = parse_generic_params_in_angles();

        // parse where clause if it exists
        AST::WhereClause where_clause = parse_where_clause();

        if (!skip_token(LEFT_CURLY)) {
            skip_after_end_block();
            return NULL;
        }

        // parse actual enum variant definitions
        ::std::vector< ::std::unique_ptr<AST::EnumItem> > enum_items = parse_enum_items();

        if (!skip_token(RIGHT_CURLY)) {
            skip_after_end_block();
            return NULL;
        }

        return ::std::unique_ptr<AST::Enum>(
          new AST::Enum(::std::move(enum_name), ::std::move(vis), ::std::move(generic_params),
            ::std::move(where_clause), ::std::move(enum_items), ::std::move(outer_attrs), locus));
    }

    // Parses the enum variants inside an enum definiton.
    ::std::vector< ::std::unique_ptr<AST::EnumItem> > Parser::parse_enum_items() {
        ::std::vector< ::std::unique_ptr<AST::EnumItem> > items;

        ::std::unique_ptr<AST::EnumItem> initial_item = parse_enum_item();

        // Return empty item list if no field there
        if (initial_item == NULL) {
            return items;
        }

        items.push_back(::std::move(initial_item));

        // maybe think of a better control structure here - do-while with an initial error state?
        // basically, loop through item list until can't find any more params
        while (true) {
            if (lexer.peek_token()->get_id() != COMMA) {
                break;
            }

            // skip comma if applies
            lexer.skip_token();

            ::std::unique_ptr<AST::EnumItem> item = parse_enum_item();

            if (item != NULL) {
                items.push_back(::std::move(item));
            } else {
                // this would occur with a trailing comma, which is allowed
                break;
            }
        }

        // TODO: does this need move?
        return items;

        // TODO: shares virtually all code with function params, tuple and struct fields - templates?
    }

    // Parses a single enum variant item in an enum definition. Does not parse commas.
    ::std::unique_ptr<AST::EnumItem> Parser::parse_enum_item() {
        // parse outer attributes if they exist
        ::std::vector<AST::Attribute> outer_attrs = parse_outer_attributes();

        // parse name for enum item, which is required
        const_TokenPtr item_name_tok = lexer.peek_token();
        if (item_name_tok->get_id() != IDENTIFIER) {
            // this may not be an error but it means there is no enum item here
            return NULL;
        }
        lexer.skip_token();
        Identifier item_name = item_name_tok->get_str();

        // branch based on next token
        const_TokenPtr t = lexer.peek_token();
        switch (t->get_id()) {
            case LEFT_PAREN: {
                // tuple enum item
                lexer.skip_token();

                ::std::vector<AST::TupleField> tuple_fields = parse_tuple_fields();

                if (!skip_token(RIGHT_PAREN)) {
                    // skip after somewhere
                    return NULL;
                }

                return ::std::unique_ptr<AST::EnumItemTuple>(
                  new AST::EnumItemTuple(::std::move(item_name), ::std::move(tuple_fields),
                    ::std::move(outer_attrs), item_name_tok->get_locus()));
            }
            case LEFT_CURLY: {
                // struct enum item
                lexer.skip_token();

                ::std::vector<AST::StructField> struct_fields = parse_struct_fields();

                if (!skip_token(RIGHT_CURLY)) {
                    // skip after somewhere
                    return NULL;
                }

                return ::std::unique_ptr<AST::EnumItemStruct>(
                  new AST::EnumItemStruct(::std::move(item_name), ::std::move(struct_fields),
                    ::std::move(outer_attrs), item_name_tok->get_locus()));
            }
            case EQUAL: {
                // discriminant enum item
                lexer.skip_token();

                ::std::unique_ptr<AST::Expr> discriminant_expr = parse_expr();

                return ::std::unique_ptr<AST::EnumItemDiscriminant>(new AST::EnumItemDiscriminant(
                  ::std::move(item_name), ::std::move(discriminant_expr), ::std::move(outer_attrs),
                  item_name_tok->get_locus()));
            }
            default:
                // regular enum with just an identifier
                return ::std::unique_ptr<AST::EnumItem>(new AST::EnumItem(
                  ::std::move(item_name), ::std::move(outer_attrs), item_name_tok->get_locus()));
        }
    }

    // Parses a C-style (and C-compat) untagged union declaration.
    ::std::unique_ptr<AST::Union> Parser::parse_union(
      AST::Visibility vis, ::std::vector<AST::Attribute> outer_attrs) {
        // hack - "weak keyword" by finding identifier called "union" (lookahead in item switch)
        location_t locus = lexer.peek_token()->get_locus();
        // skip union "identifier"
        skip_token(IDENTIFIER);

        // parse actual union name
        const_TokenPtr union_name_tok = expect_token(IDENTIFIER);
        if (union_name_tok == NULL) {
            skip_after_next_block();
            return NULL;
        }
        Identifier union_name = union_name_tok->get_str();

        // parse optional generic parameters
        ::std::vector< ::std::unique_ptr<AST::GenericParam> > generic_params
          = parse_generic_params_in_angles();

        // parse optional where clause
        AST::WhereClause where_clause = parse_where_clause();

        if (!skip_token(LEFT_CURLY)) {
            skip_after_end_block();
            return NULL;
        }

        // parse union inner items as "struct fields" because hey, syntax reuse. Spec said so.
        ::std::vector<AST::StructField> union_fields = parse_struct_fields();

        if (!skip_token(RIGHT_CURLY)) {
            // skip after somewhere
            return NULL;
        }

        return ::std::unique_ptr<AST::Union>(
          new AST::Union(::std::move(union_name), ::std::move(vis), ::std::move(generic_params),
            ::std::move(where_clause), ::std::move(union_fields), ::std::move(outer_attrs), locus));
    }

    // Parses a "constant item" (compile-time constant to maybe "inline" throughout the program).
    ::std::unique_ptr<AST::ConstantItem> Parser::parse_const_item(
      AST::Visibility vis, ::std::vector<AST::Attribute> outer_attrs) {
        location_t locus = lexer.peek_token()->get_locus();
        skip_token(CONST);

        // get constant identifier - this is either a proper identifier or the _ wildcard
        const_TokenPtr ident_tok = lexer.peek_token();
        // make default identifier the underscore wildcard one
        ::std::string ident("_");
        switch (ident_tok->get_id()) {
            case IDENTIFIER:
                ident = ident_tok->get_str();
                lexer.skip_token();
                break;
            case UNDERSCORE:
                // do nothing - identifier is already "_"
                lexer.skip_token();
                break;
            default:
                error_at(ident_tok->get_locus(),
                  "expected item name (identifier or '_') in constant item declaration - found '%s'",
                  ident_tok->get_token_description());
                skip_after_semicolon();
                return NULL;
        }

        if (!skip_token(COLON)) {
            skip_after_semicolon();
            return NULL;
        }

        // parse constant type (required)
        ::std::unique_ptr<AST::Type> type = parse_type();

        if (!skip_token(EQUAL)) {
            skip_after_semicolon();
            return NULL;
        }

        // parse constant expression (required)
        ::std::unique_ptr<AST::Expr> expr = parse_expr();

        if (!skip_token(SEMICOLON)) {
            // skip somewhere?
            return NULL;
        }

        return ::std::unique_ptr<AST::ConstantItem>(new AST::ConstantItem(::std::move(ident),
          ::std::move(vis), ::std::move(type), ::std::move(expr), ::std::move(outer_attrs), locus));
    }

    // Parses a "static item" (static storage item, with 'static lifetime).
    ::std::unique_ptr<AST::StaticItem> Parser::parse_static_item(
      AST::Visibility vis, ::std::vector<AST::Attribute> outer_attrs) {
        location_t locus = lexer.peek_token()->get_locus();
        skip_token(STATIC_TOK);

        // determine whether static item is mutable
        bool is_mut = false;
        if (lexer.peek_token()->get_id() == MUT) {
            is_mut = true;
            lexer.skip_token();
        }

        const_TokenPtr ident_tok = expect_token(IDENTIFIER);
        Identifier ident = ident_tok->get_str();

        if (!skip_token(COLON)) {
            skip_after_semicolon();
            return NULL;
        }

        // parse static item type (required)
        ::std::unique_ptr<AST::Type> type = parse_type();

        if (!skip_token(EQUAL)) {
            skip_after_semicolon();
            return NULL;
        }

        // parse static item expression (required)
        ::std::unique_ptr<AST::Expr> expr = parse_expr();

        if (!skip_token(SEMICOLON)) {
            // skip after somewhere
            return NULL;
        }

        return ::std::unique_ptr<AST::StaticItem>(new AST::StaticItem(::std::move(ident), is_mut,
          ::std::move(type), ::std::move(expr), ::std::move(vis), ::std::move(outer_attrs), locus));
    }

    // Parses a trait definition item, including unsafe ones.
    ::std::unique_ptr<AST::Trait> Parser::parse_trait(
      AST::Visibility vis, ::std::vector<AST::Attribute> outer_attrs) {
        location_t locus = lexer.peek_token()->get_locus();
        bool is_unsafe = false;
        if (lexer.peek_token()->get_id() == UNSAFE) {
            is_unsafe = true;
            lexer.skip_token();
        }

        skip_token(TRAIT);

        // parse trait name
        const_TokenPtr ident_tok = expect_token(IDENTIFIER);
        Identifier ident = ident_tok->get_str();

        // parse generic parameters (if they exist)
        ::std::vector< ::std::unique_ptr<AST::GenericParam> > generic_params
          = parse_generic_params_in_angles();

        // create placeholder type param bounds in case they don't exist
        ::std::vector< ::std::unique_ptr<AST::TypeParamBound> > type_param_bounds;

        // parse type param bounds (if they exist)
        if (lexer.peek_token()->get_id() == COLON) {
            lexer.skip_token();

            // TODO: does this need move?
            type_param_bounds = parse_type_param_bounds();
        }

        // parse where clause (if it exists)
        AST::WhereClause where_clause = parse_where_clause();

        if (!skip_token(LEFT_CURLY)) {
            skip_after_end_block();
            return NULL;
        }

        // parse trait items
        ::std::vector< ::std::unique_ptr<AST::TraitItem> > trait_items;

        const_TokenPtr t = lexer.peek_token();
        while (t->get_id() != RIGHT_CURLY) {
            ::std::unique_ptr<AST::TraitItem> trait_item = parse_trait_item();

            if (trait_item == NULL) {
                // TODO: this is probably an error as next character should equal RIGHT_CURLY
                break;
            }

            trait_items.push_back(::std::move(trait_item));

            t = lexer.peek_token();
        }

        if (!skip_token(RIGHT_CURLY)) {
            // skip after something
            return NULL;
        }

        return ::std::unique_ptr<AST::Trait>(new AST::Trait(::std::move(ident), is_unsafe,
          ::std::move(generic_params), ::std::move(type_param_bounds), ::std::move(where_clause),
          ::std::move(trait_items), ::std::move(vis), ::std::move(outer_attrs), locus));
    }

    // Parses a trait item used inside traits (not trait, the Item).
    ::std::unique_ptr<AST::TraitItem> Parser::parse_trait_item() {
        // parse outer attributes (if they exist)
        ::std::vector<AST::Attribute> outer_attrs = parse_outer_attributes();

        // lookahead to determine what type of trait item to parse
        const_TokenPtr tok = lexer.peek_token();
        switch (tok->get_id()) {
            case TYPE:
                return parse_trait_type(::std::move(outer_attrs));
            case CONST:
                // disambiguate with function qualifier
                if (lexer.peek_token(1)->get_id() == IDENTIFIER) {
                    return parse_trait_const(::std::move(outer_attrs));
                }
                // else, fallthrough to function
                // TODO: find out how to disable gcc "implicit fallthrough" error
                gcc_fallthrough();
            case UNSAFE:
            case EXTERN_TOK:
            case FN_TOK: {
                /* function and method can't be disambiguated by lookahead alone (without a lot of
                 * work and waste), so either make a "parse_trait_function_or_method" or parse here
                 * mostly and pass in most parameters (or if short enough, parse whole thing here). */
                // parse function and method here

                // parse function or method qualifiers
                AST::FunctionQualifiers qualifiers = parse_function_qualifiers();

                skip_token(FN_TOK);

                // parse function or method name
                const_TokenPtr ident_tok = expect_token(IDENTIFIER);
                Identifier ident = ident_tok->get_str();

                // parse generic params
                ::std::vector< ::std::unique_ptr<AST::GenericParam> > generic_params
                  = parse_generic_params_in_angles();

                if (!skip_token(LEFT_PAREN)) {
                    // skip after somewhere?
                    return NULL;
                }

                // now for function vs method disambiguation - method has opening "self" param
                AST::SelfParam self_param = parse_self_param();
                // FIXME: ensure that self param doesn't accidently consume tokens for a function
                bool is_method = false;
                if (!self_param.is_error()) {
                    is_method = true;

                    // skip comma so function and method regular params can be parsed in same way
                    if (lexer.peek_token()->get_id() == COMMA) {
                        lexer.skip_token();
                    }
                }

                // parse trait function params
                ::std::vector<AST::FunctionParam> function_params = parse_function_params();

                if (!skip_token(RIGHT_PAREN)) {
                    // skip after somewhere?
                    return NULL;
                }

                // parse return type (optional)
                ::std::unique_ptr<AST::Type> return_type = parse_function_return_type();

                // parse where clause (optional)
                AST::WhereClause where_clause = parse_where_clause();

                // parse semicolon or function definition (in block)
                const_TokenPtr t = lexer.peek_token();
                ::std::unique_ptr<AST::BlockExpr> definition = NULL;
                switch (t->get_id()) {
                    case SEMICOLON:
                        lexer.skip_token();
                        // definition is already NULL, so don't need to change it
                        break;
                    case LEFT_CURLY:
                        definition = parse_block_expr();
                        // FIXME: are these outer attributes meant to be passed into the block?
                        break;
                    default:
                        error_at(t->get_locus(),
                          "expected ';' or definiton at the end of trait %s definition - found '%s' "
                          "instead",
                          is_method ? "method" : "function", t->get_token_description());
                        // skip?
                        return NULL;
                }

                // do actual if instead of ternary for return value optimisation
                if (is_method) {
                    AST::TraitMethodDecl method_decl(::std::move(ident), ::std::move(qualifiers),
                      ::std::move(generic_params), ::std::move(self_param),
                      ::std::move(function_params), ::std::move(return_type),
                      ::std::move(where_clause));

                    // TODO: does this (method_decl) need move?
                    return ::std::unique_ptr<AST::TraitItemMethod>(
                      new AST::TraitItemMethod(::std::move(method_decl), ::std::move(definition),
                        ::std::move(outer_attrs), tok->get_locus()));
                } else {
                    AST::TraitFunctionDecl function_decl(::std::move(ident), ::std::move(qualifiers),
                      ::std::move(generic_params), ::std::move(function_params),
                      ::std::move(return_type), ::std::move(where_clause));

                    return ::std::unique_ptr<AST::TraitItemFunc>(
                      new AST::TraitItemFunc(::std::move(function_decl), ::std::move(definition),
                        ::std::move(outer_attrs), tok->get_locus()));
                }
            }
            default: {
                // TODO: try and parse macro invocation semi - if fails, maybe error.
                ::std::unique_ptr<AST::MacroInvocationSemi> macro_invoc
                  = parse_macro_invocation_semi(outer_attrs);

                if (macro_invoc == NULL) {
                    // TODO: error?
                    return NULL;
                } else {
                    return macro_invoc;
                }
                // FIXME: macro invocations can only start with certain tokens. be more picky with
                // these?
            }
        }
    }

    // Parse a typedef trait item.
    ::std::unique_ptr<AST::TraitItemType> Parser::parse_trait_type(
      ::std::vector<AST::Attribute> outer_attrs) {
        location_t locus = lexer.peek_token()->get_locus();
        skip_token(TYPE);

        const_TokenPtr ident_tok = expect_token(IDENTIFIER);
        Identifier ident = ident_tok->get_str();

        bool has_colon = false;
        ::std::vector< ::std::unique_ptr<AST::TypeParamBound> > bounds;

        // parse optional colon
        if (lexer.peek_token()->get_id() == COLON) {
            has_colon = true;
            lexer.skip_token();

            // parse optional type param bounds
            bounds = parse_type_param_bounds();
        }

        if (!skip_token(SEMICOLON)) {
            // skip?
            return NULL;
        }

        return ::std::unique_ptr<AST::TraitItemType>(new AST::TraitItemType(
          ::std::move(ident), ::std::move(bounds), ::std::move(outer_attrs), locus));
    }

    // Parses a constant trait item.
    ::std::unique_ptr<AST::TraitItemConst> Parser::parse_trait_const(
      ::std::vector<AST::Attribute> outer_attrs) {
        location_t locus = lexer.peek_token()->get_locus();
        skip_token(CONST);

        // parse constant item name
        const_TokenPtr ident_tok = expect_token(IDENTIFIER);
        Identifier ident = ident_tok->get_str();

        if (!skip_token(COLON)) {
            skip_after_semicolon();
            return NULL;
        }

        // parse constant trait item type
        ::std::unique_ptr<AST::Type> type = parse_type();

        // parse constant trait body expression, if it exists
        ::std::unique_ptr<AST::Expr> const_body = NULL;
        if (lexer.peek_token()->get_id() == EQUAL) {
            lexer.skip_token();

            // expression must exist, so parse it
            const_body = parse_expr();
        }

        if (!skip_token(SEMICOLON)) {
            // skip after something?
            return NULL;
        }

        return ::std::unique_ptr<AST::TraitItemConst>(new AST::TraitItemConst(::std::move(ident),
          ::std::move(type), ::std::move(const_body), ::std::move(outer_attrs), locus));
    }

    // Parses a struct "impl" item (both inherent impl and trait impl can be parsed here),
    ::std::unique_ptr<AST::Impl> Parser::parse_impl(
      AST::Visibility vis, ::std::vector<AST::Attribute> outer_attrs) {
        /* Note that only trait impls are allowed to be unsafe. So if unsafe, it must be a trait
         * impl. However, this isn't enough for full disambiguation, so don't branch here. */
        location_t locus = lexer.peek_token()->get_locus();
        bool is_unsafe = false;
        if (lexer.peek_token()->get_id() == UNSAFE) {
            lexer.skip_token();
            is_unsafe = true;
        }

        if (!skip_token(IMPL)) {
            skip_after_next_block();
            return NULL;
        }

        // parse generic params (shared by trait and inherent impls)
        ::std::vector< ::std::unique_ptr<AST::GenericParam> > generic_params
          = parse_generic_params_in_angles();

        // Again, trait impl-only feature, but optional one, so can be used for branching yet.
        bool has_exclam = false;
        if (lexer.peek_token()->get_id() == EXCLAM) {
            lexer.skip_token();
            has_exclam = true;
        }

        /* FIXME: code that doesn't look shit for TypePath. Also, make sure this doesn't parse too
         * much and not work. */
        AST::TypePath type_path = parse_type_path();
        if (type_path.is_error() || lexer.peek_token()->get_id() != FOR) {
            // cannot parse type path (or not for token next, at least), so must be inherent impl

            // hacky conversion of TypePath stack object to Type pointer
            ::std::unique_ptr<AST::Type> type = NULL;
            if (!type_path.is_error()) {
                // TODO: would move work here?
                type = ::std::unique_ptr<AST::TypePath>(new AST::TypePath(type_path));
            } else {
                type = parse_type();
            }
            // Type is required, so error if null
            if (type == NULL) {
                error_at(lexer.peek_token()->get_locus(), "could not parse type in inherent impl");
                skip_after_next_block();
                return NULL;
            }

            // parse optional where clause
            AST::WhereClause where_clause = parse_where_clause();

            if (!skip_token(LEFT_CURLY)) {
                // TODO: does this still skip properly?
                skip_after_end_block();
                return NULL;
            }

            // parse inner attributes (optional)
            ::std::vector<AST::Attribute> inner_attrs = parse_inner_attributes();

            // parse inherent impl items
            ::std::vector< ::std::unique_ptr<AST::InherentImplItem> > impl_items;

            const_TokenPtr t = lexer.peek_token();
            while (t->get_id() != RIGHT_CURLY) {
                ::std::unique_ptr<AST::InherentImplItem> impl_item = parse_inherent_impl_item();

                if (impl_item == NULL) {
                    // TODO: this is probably an error as next character should equal RIGHT_CURLY
                    fprintf(stderr,
                      "impl item is null and next char wasn't RIGHT_CURLY - probably an error");
                    break;
                }

                impl_items.push_back(::std::move(impl_item));

                t = lexer.peek_token();
            }

            if (!skip_token(RIGHT_CURLY)) {
                // skip somewhere
                return NULL;
            }

            // DEBUG
            fprintf(stderr, "successfully parsed inherent impl\n");

            return ::std::unique_ptr<AST::InherentImpl>(new AST::InherentImpl(::std::move(impl_items),
              ::std::move(generic_params), ::std::move(type), ::std::move(where_clause),
              ::std::move(vis), ::std::move(inner_attrs), ::std::move(outer_attrs), locus));
        } else {
            // type path must both be valid and next token is for, so trait impl
            if (!skip_token(FOR)) {
                skip_after_next_block();
                return NULL;
            }

            // parse type
            ::std::unique_ptr<AST::Type> type = parse_type();
            // ensure type is included as it is required
            if (type == NULL) {
                error_at(lexer.peek_token()->get_locus(), "could not parse type in trait impl");
                skip_after_next_block();
                return NULL;
            }

            // parse optional where clause
            AST::WhereClause where_clause = parse_where_clause();

            if (!skip_token(LEFT_CURLY)) {
                // TODO: does this still skip properly?
                skip_after_end_block();
                return NULL;
            }

            // parse inner attributes (optional)
            ::std::vector<AST::Attribute> inner_attrs = parse_inner_attributes();

            // parse trait impl items
            ::std::vector< ::std::unique_ptr<AST::TraitImplItem> > impl_items;

            const_TokenPtr t = lexer.peek_token();
            while (t->get_id() != RIGHT_CURLY) {
                ::std::unique_ptr<AST::TraitImplItem> impl_item = parse_trait_impl_item();

                if (impl_item == NULL) {
                    // DEBUG
                    fprintf(
                      stderr, "break out of parsing trait impl items (due to parse giving null)\n");

                    // TODO: this is probably an error as next character should equal RIGHT_CURLY
                    break;
                }

                impl_items.push_back(::std::move(impl_item));

                t = lexer.peek_token();

                // DEBUG
                fprintf(stderr, "successfully parsed a trait impl item\n");
            }
            // DEBUG
            fprintf(stderr, "successfully finished trait impl items\n");

            if (!skip_token(RIGHT_CURLY)) {
                // skip somewhere
                return NULL;
            }

            // DEBUG
            fprintf(stderr, "successfully parsed trait impl\n");

            return ::std::unique_ptr<AST::TraitImpl>(new AST::TraitImpl(::std::move(type_path),
              is_unsafe, has_exclam, ::std::move(impl_items), ::std::move(generic_params),
              ::std::move(type), ::std::move(where_clause), ::std::move(vis),
              ::std::move(inner_attrs), ::std::move(outer_attrs), locus));
        }
    }

    // Parses a single inherent impl item (item inside an inherent impl block).
    ::std::unique_ptr<AST::InherentImplItem> Parser::parse_inherent_impl_item() {
        // parse outer attributes (if they exist)
        ::std::vector<AST::Attribute> outer_attrs = parse_outer_attributes();

        // TODO: cleanup - currently an unreadable mess

        // branch on next token:
        const_TokenPtr t = lexer.peek_token();
        switch (t->get_id()) {
            case IDENTIFIER:
            case SUPER:
            case SELF:
            case CRATE:
            case DOLLAR_SIGN:
                // these seem to be SimplePath tokens, so this is a macro invocation semi
                return parse_macro_invocation_semi(::std::move(outer_attrs));
            case PUB: {
                // visibility, so not a macro invocation semi - must be constant, function, or method
                AST::Visibility vis = parse_visibility();

                // TODO: is a recursive call to parse_inherent_impl_item better?
                switch (lexer.peek_token()->get_id()) {
                    case EXTERN_TOK:
                    case UNSAFE:
                    case FN_TOK:
                        // function or method
                        return parse_inherent_impl_function_or_method(
                          ::std::move(vis), ::std::move(outer_attrs));
                    case CONST:
                        // lookahead to resolve production - could be function/method or const item
                        t = lexer.peek_token(1);

                        switch (t->get_id()) {
                            case IDENTIFIER:
                            case UNDERSCORE:
                                return parse_const_item(::std::move(vis), ::std::move(outer_attrs));
                            case UNSAFE:
                            case EXTERN_TOK:
                            case FN_TOK:
                                return parse_inherent_impl_function_or_method(
                                  ::std::move(vis), ::std::move(outer_attrs));
                            default:
                                error_at(t->get_locus(),
                                  "unexpected token '%s' in some sort of const item in inherent impl",
                                  t->get_token_description());
                                lexer.skip_token(1); // TODO: is this right thing to do?
                                return NULL;
                        }
                    default:
                        error_at(t->get_locus(), "unrecognised token '%s' for item in inherent impl",
                          t->get_token_description());
                        // skip?
                        return NULL;
                }
            }
            case EXTERN_TOK:
            case UNSAFE:
            case FN_TOK:
                // function or method
                return parse_inherent_impl_function_or_method(
                  AST::Visibility::create_error(), ::std::move(outer_attrs));
            case CONST:
                // lookahead to resolve production - could be function/method or const item
                t = lexer.peek_token(1);

                switch (t->get_id()) {
                    case IDENTIFIER:
                    case UNDERSCORE:
                        return parse_const_item(
                          AST::Visibility::create_error(), ::std::move(outer_attrs));
                    case UNSAFE:
                    case EXTERN_TOK:
                    case FN_TOK:
                        return parse_inherent_impl_function_or_method(
                          AST::Visibility::create_error(), ::std::move(outer_attrs));
                    default:
                        error_at(t->get_locus(),
                          "unexpected token '%s' in some sort of const item in inherent impl",
                          t->get_token_description());
                        lexer.skip_token(1); // TODO: is this right thing to do?
                        return NULL;
                }
                gcc_unreachable();
            default:
                error_at(t->get_locus(), "unrecognised token '%s' for item in inherent impl",
                  t->get_token_description());
                // skip?
                return NULL;
        }
    }

    /* For internal use only by parse_inherent_impl_item() - splits giant method into smaller ones
     * and prevents duplication of logic. Strictly, this parses a function or method item inside an
     * inherent impl item block. */
    // TODO: make this a templated function with "return type" as type param - InherentImplItem is this 
    // specialisation of the template while TraitImplItem will be the other.
    ::std::unique_ptr<AST::InherentImplItem> Parser::parse_inherent_impl_function_or_method(
      AST::Visibility vis, ::std::vector<AST::Attribute> outer_attrs) {
        location_t locus = lexer.peek_token()->get_locus();
        // parse function or method qualifiers
        AST::FunctionQualifiers qualifiers = parse_function_qualifiers();

        skip_token(FN_TOK);

        // parse function or method name
        const_TokenPtr ident_tok = expect_token(IDENTIFIER);
        Identifier ident = ident_tok->get_str();

        // parse generic params
        ::std::vector< ::std::unique_ptr<AST::GenericParam> > generic_params
          = parse_generic_params_in_angles();

        if (!skip_token(LEFT_PAREN)) {
            // skip after somewhere?
            return NULL;
        }

        // now for function vs method disambiguation - method has opening "self" param
        AST::SelfParam self_param = parse_self_param();
        // FIXME: ensure that self param doesn't accidently consume tokens for a function
        // one idea is to lookahead up to 4 tokens to see whether self is one of them
        bool is_method = false;
        if (!self_param.is_error()) {
            is_method = true;

            // skip comma so function and method regular params can be parsed in same way
            if (lexer.peek_token()->get_id() == COMMA) {
                lexer.skip_token();
            }
        }

        // parse trait function params
        ::std::vector<AST::FunctionParam> function_params = parse_function_params();

        if (!skip_token(RIGHT_PAREN)) {
            skip_after_end_block();
            return NULL;
        }

        // parse return type (optional)
        ::std::unique_ptr<AST::Type> return_type = parse_function_return_type();

        // parse where clause (optional)
        AST::WhereClause where_clause = parse_where_clause();

        // parse function definition (in block) - semicolon not allowed
        if (lexer.peek_token()->get_id() == SEMICOLON) {
            error_at(lexer.peek_token()->get_locus(),
              "%s declaration in inherent impl not allowed - must have a definition",
              is_method ? "method" : "function");
            lexer.skip_token();
            return NULL;
        }
        ::std::unique_ptr<AST::BlockExpr> body = parse_block_expr();
        if (body == NULL) {
            error_at(lexer.peek_token()->get_locus(),
              "could not parse definition in inherent impl %s definition",
              is_method ? "method" : "function");
            skip_after_end_block();
            return NULL;
        }

        // do actual if instead of ternary for return value optimisation
        if (is_method) {
            return ::std::unique_ptr<AST::Method>(new AST::Method(::std::move(ident),
              ::std::move(qualifiers), ::std::move(generic_params), ::std::move(self_param),
              ::std::move(function_params), ::std::move(return_type), ::std::move(where_clause),
              ::std::move(body), ::std::move(vis), ::std::move(outer_attrs), locus));
        } else {
            return ::std::unique_ptr<AST::Function>(new AST::Function(::std::move(ident),
              ::std::move(qualifiers), ::std::move(generic_params), ::std::move(function_params),
              ::std::move(return_type), ::std::move(where_clause), ::std::move(body),
              ::std::move(vis), ::std::move(outer_attrs), locus));
        }
    }

    // Parses a single trait impl item (item inside a trait impl block).
    ::std::unique_ptr<AST::TraitImplItem> Parser::parse_trait_impl_item() {
        // parse outer attributes (if they exist)
        ::std::vector<AST::Attribute> outer_attrs = parse_outer_attributes();

        // TODO: clean this function up, it is basically unreadable hacks

        // branch on next token:
        const_TokenPtr t = lexer.peek_token();
        switch (t->get_id()) {
            case IDENTIFIER:
            case SUPER:
            case SELF:
            case CRATE:
            case DOLLAR_SIGN:
                // these seem to be SimplePath tokens, so this is a macro invocation semi
                return parse_macro_invocation_semi(::std::move(outer_attrs));
            case TYPE:
                return parse_type_alias(AST::Visibility::create_error(), ::std::move(outer_attrs));
            case PUB: {
                // visibility, so not a macro invocation semi - must be constant, function, or method
                AST::Visibility vis = parse_visibility();

                // TODO: is a recursive call to parse_trait_impl_item better?
                switch (lexer.peek_token()->get_id()) {
                    case TYPE:
                        return parse_type_alias(::std::move(vis), ::std::move(outer_attrs));
                    case EXTERN_TOK:
                    case UNSAFE:
                    case FN_TOK:
                        // function or method
                        return parse_trait_impl_function_or_method(
                          ::std::move(vis), ::std::move(outer_attrs));
                    case CONST:
                        // lookahead to resolve production - could be function/method or const item
                        t = lexer.peek_token(1);

                        switch (t->get_id()) {
                            case IDENTIFIER:
                            case UNDERSCORE:
                                return parse_const_item(::std::move(vis), ::std::move(outer_attrs));
                            case UNSAFE:
                            case EXTERN_TOK:
                            case FN_TOK:
                                return parse_trait_impl_function_or_method(
                                  ::std::move(vis), ::std::move(outer_attrs));
                            default:
                                error_at(t->get_locus(),
                                  "unexpected token '%s' in some sort of const item in trait impl",
                                  t->get_token_description());
                                lexer.skip_token(1); // TODO: is this right thing to do?
                                return NULL;
                        }
                    default:
                        error_at(t->get_locus(), "unrecognised token '%s' for item in trait impl",
                          t->get_token_description());
                        // skip?
                        return NULL;
                }
            }
            case EXTERN_TOK:
            case UNSAFE:
            case FN_TOK:
                // function or method
                return parse_trait_impl_function_or_method(
                  AST::Visibility::create_error(), ::std::move(outer_attrs));
            case CONST:
                // lookahead to resolve production - could be function/method or const item
                t = lexer.peek_token(1);

                switch (t->get_id()) {
                    case IDENTIFIER:
                    case UNDERSCORE:
                        return parse_const_item(
                          AST::Visibility::create_error(), ::std::move(outer_attrs));
                    case UNSAFE:
                    case EXTERN_TOK:
                    case FN_TOK:
                        return parse_trait_impl_function_or_method(
                          AST::Visibility::create_error(), ::std::move(outer_attrs));
                    default:
                        error_at(t->get_locus(),
                          "unexpected token '%s' in some sort of const item in trait impl",
                          t->get_token_description());
                        lexer.skip_token(1); // TODO: is this right thing to do?
                        return NULL;
                }
                gcc_unreachable();
            default:
                error_at(t->get_locus(), "unrecognised token '%s' for item in trait impl",
                  t->get_token_description());
                // skip?
                return NULL;
        }
    }

    /* For internal use only by parse_trait_impl_item() - splits giant method into smaller ones
     * and prevents duplication of logic. Strictly, this parses a function or method item inside a
     * trait impl item block. */
    ::std::unique_ptr<AST::TraitImplItem> Parser::parse_trait_impl_function_or_method(
      AST::Visibility vis, ::std::vector<AST::Attribute> outer_attrs) {
        // this shares virtually all logic with parse_inherent_impl_function_or_method - template?
        location_t locus = lexer.peek_token()->get_locus();

        // parse function or method qualifiers
        AST::FunctionQualifiers qualifiers = parse_function_qualifiers();

        skip_token(FN_TOK);

        // parse function or method name
        const_TokenPtr ident_tok = expect_token(IDENTIFIER);
        if (ident_tok == NULL) {
            return NULL;
        }
        Identifier ident = ident_tok->get_str();

        // DEBUG:
        fprintf(stderr, "about to start parsing generic params in trait impl function or method\n");

        // parse generic params
        ::std::vector< ::std::unique_ptr<AST::GenericParam> > generic_params
          = parse_generic_params_in_angles();

        // DEBUG:
        fprintf(stderr, "finished parsing generic params in trait impl function or method\n");

        if (!skip_token(LEFT_PAREN)) {
            // skip after somewhere?
            return NULL;
        }

        // now for function vs method disambiguation - method has opening "self" param
        AST::SelfParam self_param = parse_self_param();
        // FIXME: ensure that self param doesn't accidently consume tokens for a function
        bool is_method = false;
        if (!self_param.is_error()) {
            is_method = true;

            // skip comma so function and method regular params can be parsed in same way
            if (lexer.peek_token()->get_id() == COMMA) {
                lexer.skip_token();
            }

            // DEBUG
            fprintf(stderr, "successfully parsed self param in method trait impl item\n");
        }

        // DEBUG
        fprintf(stderr, "started to parse function params in function or method trait impl item\n");

        // parse trait function params (only if next token isn't right paren)
        ::std::vector<AST::FunctionParam> function_params;
        if (lexer.peek_token()->get_id() != RIGHT_PAREN) {
            function_params = parse_function_params();

            if (function_params.empty()) {
                error_at(lexer.peek_token()->get_locus(),
                  "failed to parse function params in trait impl %s definition",
                  is_method ? "method" : "function");
                skip_after_next_block();
                return NULL;
            }
        }

        // FIXME: segfault occurs during parsing of function params

        // DEBUG
        fprintf(
          stderr, "successfully parsed function params in function or method trait impl item\n");

        if (!skip_token(RIGHT_PAREN)) {
            skip_after_next_block();
            return NULL;
        }

        // parse return type (optional)
        ::std::unique_ptr<AST::Type> return_type = parse_function_return_type();

        // DEBUG
        fprintf(stderr, "successfully parsed return type in function or method trait impl item\n");

        // parse where clause (optional)
        AST::WhereClause where_clause = parse_where_clause();

        // DEBUG
        fprintf(stderr, "successfully parsed where clause in function or method trait impl item\n");

        // parse function definition (in block) - semicolon not allowed
        if (lexer.peek_token()->get_id() == SEMICOLON) {
            error_at(lexer.peek_token()->get_locus(),
              "%s declaration in trait impl not allowed - must have a definition",
              is_method ? "method" : "function");
            lexer.skip_token();
            return NULL;
        }
        ::std::unique_ptr<AST::BlockExpr> body = parse_block_expr();
        if (body == NULL) {
            error_at(lexer.peek_token()->get_locus(),
              "could not parse definition in trait impl %s definition",
              is_method ? "method" : "function");
            skip_after_end_block();
            return NULL;
        }

        // do actual if instead of ternary for return value optimisation
        if (is_method) {
            return ::std::unique_ptr<AST::Method>(new AST::Method(::std::move(ident),
              ::std::move(qualifiers), ::std::move(generic_params), ::std::move(self_param),
              ::std::move(function_params), ::std::move(return_type), ::std::move(where_clause),
              ::std::move(body), ::std::move(vis), ::std::move(outer_attrs), locus));
        } else {
            return ::std::unique_ptr<AST::Function>(new AST::Function(::std::move(ident),
              ::std::move(qualifiers), ::std::move(generic_params), ::std::move(function_params),
              ::std::move(return_type), ::std::move(where_clause), ::std::move(body),
              ::std::move(vis), ::std::move(outer_attrs), locus));
        }
    }

    // Parses an extern block of declarations.
    ::std::unique_ptr<AST::ExternBlock> Parser::parse_extern_block(
      AST::Visibility vis, ::std::vector<AST::Attribute> outer_attrs) {
        location_t locus = lexer.peek_token()->get_locus();
        skip_token(EXTERN_TOK);

        // detect optional abi name
        ::std::string abi;
        const_TokenPtr next_tok = lexer.peek_token();
        if (next_tok->get_id() == STRING_LITERAL) {
            lexer.skip_token();
            abi = next_tok->get_str();
        }

        if (!skip_token(LEFT_CURLY)) {
            skip_after_end_block();
            return NULL;
        }

        ::std::vector<AST::Attribute> inner_attrs = parse_inner_attributes();

        // parse declarations inside extern block
        ::std::vector< ::std::unique_ptr<AST::ExternalItem> > extern_items;

        const_TokenPtr t = lexer.peek_token();
        while (t->get_id() != RIGHT_CURLY) {
            ::std::unique_ptr<AST::ExternalItem> extern_item = parse_external_item();

            if (extern_item == NULL) {
                error_at(t->get_locus(),
                  "failed to parse external item despite not reaching end of extern block");
                return NULL;
            }

            extern_items.push_back(::std::move(extern_item));

            t = lexer.peek_token();
        }

        if (!skip_token(RIGHT_CURLY)) {
            // skip somewhere
            return NULL;
        }

        return ::std::unique_ptr<AST::ExternBlock>(
          new AST::ExternBlock(::std::move(abi), ::std::move(extern_items), ::std::move(vis),
            ::std::move(inner_attrs), ::std::move(outer_attrs), locus));
    }

    // Parses a single extern block item (static or function declaration).
    ::std::unique_ptr<AST::ExternalItem> Parser::parse_external_item() {
        // parse optional outer attributes
        ::std::vector<AST::Attribute> outer_attrs = parse_outer_attributes();

        location_t locus = lexer.peek_token()->get_locus();

        // parse optional visibility
        AST::Visibility vis = parse_visibility();

        const_TokenPtr t = lexer.peek_token();
        switch (t->get_id()) {
            case STATIC_TOK: {
                // parse extern static item
                lexer.skip_token();

                // parse mut (optional)
                bool has_mut = false;
                if (lexer.peek_token()->get_id() == MUT) {
                    lexer.skip_token();
                    has_mut = true;
                }

                // parse identifier
                const_TokenPtr ident_tok = expect_token(IDENTIFIER);
                if (ident_tok == NULL) {
                    skip_after_semicolon();
                    return NULL;
                }
                Identifier ident = ident_tok->get_str();

                if (!skip_token(COLON)) {
                    skip_after_semicolon();
                    return NULL;
                }

                // parse type (required)
                ::std::unique_ptr<AST::Type> type = parse_type();
                if (type == NULL) {
                    error_at(lexer.peek_token()->get_locus(),
                      "failed to parse type in external static item");
                    skip_after_semicolon();
                    return NULL;
                }

                if (!skip_token(SEMICOLON)) {
                    // skip after somewhere?
                    return NULL;
                }

                return ::std::unique_ptr<AST::ExternalStaticItem>(
                  new AST::ExternalStaticItem(::std::move(ident), ::std::move(type), has_mut,
                    ::std::move(vis), ::std::move(outer_attrs), locus));
            }
            case FN_TOK: {
                // parse extern function declaration item
                // skip function token
                lexer.skip_token();

                // parse identifier
                const_TokenPtr ident_tok = expect_token(IDENTIFIER);
                if (ident_tok == NULL) {
                    skip_after_semicolon();
                    return NULL;
                }
                Identifier ident = ident_tok->get_str();

                // parse (optional) generic params
                ::std::vector< ::std::unique_ptr<AST::GenericParam> > generic_params
                  = parse_generic_params_in_angles();

                if (!skip_token(LEFT_PAREN)) {
                    skip_after_semicolon();
                    return NULL;
                }

                // parse parameters
                ::std::vector<AST::NamedFunctionParam> function_params;
                bool is_variadic = false;

                const_TokenPtr t = lexer.peek_token();
                while (t->get_id() != RIGHT_PAREN) {
                    AST::NamedFunctionParam param = parse_named_function_param();

                    if (param.is_error()) {
                        // is this an error? probably
                        error_at(t->get_locus(),
                          "could not parse named function parameter in external function");
                        skip_after_semicolon();
                        return NULL;
                    }

                    function_params.push_back(::std::move(param));

                    t = lexer.peek_token();
                    if (t->get_id() != COMMA) {
                        if (t->get_id() != RIGHT_PAREN) {
                            error_at(t->get_locus(),
                              "expected comma or right parentheses in named function parameters, "
                              "found '%s'",
                              t->get_token_description());
                        } else {
                            // end of loop
                            break;
                        }
                    }
                    // skip comma
                    lexer.skip_token();

                    t = lexer.peek_token();

                    // parse variadic ... if it exists
                    if (t->get_id() == ELLIPSIS && lexer.peek_token(1)->get_id() == RIGHT_PAREN) {
                        lexer.skip_token();

                        is_variadic = true;

                        t = lexer.peek_token();
                    }
                }

                if (!skip_token(RIGHT_PAREN)) {
                    skip_after_semicolon();
                    return NULL;
                }

                // parse (optional) return type
                ::std::unique_ptr<AST::Type> return_type = parse_function_return_type();

                // parse (optional) where clause
                AST::WhereClause where_clause = parse_where_clause();

                if (!skip_token(SEMICOLON)) {
                    // skip somewhere?
                    return NULL;
                }

                return ::std::unique_ptr<AST::ExternalFunctionItem>(
                  new AST::ExternalFunctionItem(::std::move(ident), ::std::move(generic_params),
                    ::std::move(return_type), ::std::move(where_clause), ::std::move(function_params),
                    is_variadic, ::std::move(vis), ::std::move(outer_attrs), locus));
            }
            default:
                // error
                error_at(t->get_locus(), "unrecognised token '%s' in extern block item declaration",
                  t->get_token_description());
                skip_after_semicolon();
                return NULL;
        }
    }

    // Parses an extern block function param (with "pattern" being _ or an identifier).
    AST::NamedFunctionParam Parser::parse_named_function_param() {
        // parse identifier/_
        Identifier name;

        const_TokenPtr t = lexer.peek_token();
        switch (t->get_id()) {
            case IDENTIFIER:
                name = t->get_str();
                lexer.skip_token();
                break;
            case UNDERSCORE:
                name = "_";
                lexer.skip_token();
                break;
            default:
                // this is not a function param, but not necessarily an error
                return AST::NamedFunctionParam::create_error();
        }

        if (!skip_token(COLON)) {
            // skip after somewhere?
            return AST::NamedFunctionParam::create_error();
        }

        // parse (required) type
        ::std::unique_ptr<AST::Type> param_type = parse_type();
        if (param_type == NULL) {
            error_at(lexer.peek_token()->get_locus(),
              "could not parse param type in extern block function declaration");
            skip_after_semicolon();
            return AST::NamedFunctionParam::create_error();
        }

        return AST::NamedFunctionParam(::std::move(name), ::std::move(param_type));
    }

    // Parses a statement (will further disambiguate any statement).
    ::std::unique_ptr<AST::Stmt> Parser::parse_stmt() {
        // quick exit for empty statement
        const_TokenPtr t = lexer.peek_token();
        if (t->get_id() == SEMICOLON) {
            lexer.skip_token();
            return ::std::unique_ptr<AST::EmptyStmt>(new AST::EmptyStmt(t->get_locus()));
        }

        // parse outer attributes
        ::std::vector<AST::Attribute> outer_attrs = parse_outer_attributes();

        // parsing this will be annoying because of the many different possibilities
        /* best may be just to copy paste in parse_item switch, and failing that try to parse outer
         * attributes, and then pass them in to either a let statement or (fallback) expression
         * statement. */
        // FIXME: think of a way to do this without such a large switch?
        t = lexer.peek_token();
        switch (t->get_id()) {
            case LET:
                // let statement
                return parse_let_stmt(::std::move(outer_attrs));
            case PUB:
            case MOD:
            case EXTERN_TOK:
            case USE:
            case FN_TOK:
            case TYPE:
            case STRUCT_TOK:
            case ENUM_TOK:
            case CONST:
            case STATIC_TOK:
            case TRAIT:
            case IMPL:
            /* TODO: implement union keyword but not really because of context-dependence
             * crappy hack way to parse a union written below to separate it from the good code. */
            // case UNION:
            case UNSAFE: // maybe - unsafe traits are a thing
                // if any of these (should be all possible VisItem prefixes), parse a VisItem
                // can't parse item because would require reparsing outer attributes
                return parse_vis_item(::std::move(outer_attrs));
                break;
            case SUPER:
            case SELF:
            case CRATE:
            case DOLLAR_SIGN:
                // almost certainly macro invocation semi
                return parse_macro_item(::std::move(outer_attrs));
                break;
            // crappy hack to do union "keyword"
            case IDENTIFIER:
                // TODO: ensure std::string and literal comparison works
                if (t->get_str() == "union") {
                    return parse_vis_item(::std::move(outer_attrs));
                    // or should this go straight to parsing union?
                } else if (t->get_str() == "macro_rules") {
                    // macro_rules! macro item
                    return parse_macro_item(::std::move(outer_attrs));
                } else if (lexer.peek_token(1)->get_id() == SCOPE_RESOLUTION
                           || lexer.peek_token(1)->get_id() == EXCLAM) {
                    // FIXME: ensure doesn't take any expressions by mistake
                    // path (probably) or macro invocation, so probably a macro invocation semi
                    return parse_macro_item(::std::move(outer_attrs));
                }
                gcc_fallthrough();
                // TODO: find out how to disable gcc "implicit fallthrough" warning
            default:
                // fallback: expression statement
                return parse_expr_stmt(::std::move(outer_attrs));
                break;
        }
    }

    // Parses a let statement.
    ::std::unique_ptr<AST::LetStmt> Parser::parse_let_stmt(
      ::std::vector<AST::Attribute> outer_attrs) {
        location_t locus = lexer.peek_token()->get_locus();
        skip_token(LET);

        // parse pattern (required)
        ::std::unique_ptr<AST::Pattern> pattern = parse_pattern();
        if (pattern == NULL) {
            error_at(lexer.peek_token()->get_locus(), "failed to parse pattern in let statement");
            skip_after_semicolon();
            return NULL;
        }

        // parse type declaration (optional)
        ::std::unique_ptr<AST::Type> type = NULL;
        if (lexer.peek_token()->get_id() == COLON) {
            // must have a type declaration
            lexer.skip_token();

            type = parse_type();
            if (type == NULL) {
                error_at(lexer.peek_token()->get_locus(), "failed to parse type in let statement");
                skip_after_semicolon();
                return NULL;
            }
        }

        // parse expression to set variable to (optional)
        ::std::unique_ptr<AST::Expr> expr = NULL;
        if (lexer.peek_token()->get_id() == EQUAL) {
            // must have an expression
            lexer.skip_token();

            expr = parse_expr();
            if (expr == NULL) {
                error_at(
                  lexer.peek_token()->get_locus(), "failed to parse expression in let statement");
                skip_after_semicolon();
                return NULL;
            }
        }

        if (!skip_token(SEMICOLON)) {
            // skip after somewhere
            return NULL;
            // TODO: how wise is it to ditch a mostly-valid let statement just because a semicolon is
            // missing?
        }

        return ::std::unique_ptr<AST::LetStmt>(new AST::LetStmt(::std::move(pattern),
          ::std::move(expr), ::std::move(type), ::std::move(outer_attrs), locus));
    }

    // Parses a type path.
    AST::TypePath Parser::parse_type_path() {
        bool has_opening_scope_resolution = false;
        if (lexer.peek_token()->get_id() == SCOPE_RESOLUTION) {
            has_opening_scope_resolution = true;
            lexer.skip_token();
        }

        // create segment vector
        ::std::vector< ::std::unique_ptr<AST::TypePathSegment> > segments;

        // parse required initial segment
        ::std::unique_ptr<AST::TypePathSegment> initial_segment = parse_type_path_segment();
        if (initial_segment == NULL) {
            // skip after somewhere?
            // don't necessarily throw error but yeah
            return AST::TypePath::create_error();
        }
        segments.push_back(::std::move(initial_segment));

        // parse optional segments (as long as scope resolution operator exists)
        const_TokenPtr t = lexer.peek_token();
        while (t->get_id() == SCOPE_RESOLUTION) {
            // skip scope resolution operator
            lexer.skip_token();

            // parse the actual segment - it is an error if it doesn't exist now
            ::std::unique_ptr<AST::TypePathSegment> segment = parse_type_path_segment();
            if (segment == NULL) {
                // skip after somewhere?
                error_at(t->get_locus(), "could not parse type path segment");
                return AST::TypePath::create_error();
            }

            segments.push_back(::std::move(segment));

            t = lexer.peek_token();
        }

        return AST::TypePath(::std::move(segments), has_opening_scope_resolution);
    }

    // Parses the generic arguments in each path segment.
    AST::GenericArgs Parser::parse_path_generic_args() {
        if (!skip_token(LEFT_ANGLE)) {
            // skip after somewhere?
            return AST::GenericArgs::create_empty();
        }

        // try to parse lifetimes first
        ::std::vector<AST::Lifetime> lifetime_args;

        const_TokenPtr t = lexer.peek_token();
        location_t locus = t->get_locus();
        const_TokenPtr t2 = lexer.peek_token(1);
        while (
          t->get_id() == LIFETIME && (t2->get_id() == COMMA || !is_right_angle_tok(t2->get_id()))) {
            AST::Lifetime lifetime = parse_lifetime();
            if (lifetime.is_error()) {
                // not necessarily an error
                break;
            }

            lifetime_args.push_back(::std::move(lifetime));

            // if next token isn't comma, then it must be end of list
            if (lexer.peek_token()->get_id() != COMMA) {
                break;
            }
            // skip comma
            lexer.skip_token();

            t = lexer.peek_token();
            t2 = lexer.peek_token(1);
        }

        // try to parse types second
        ::std::vector< ::std::unique_ptr<AST::Type> > type_args;

        // TODO: think of better control structure
        t = lexer.peek_token();
        while (!is_right_angle_tok(t->get_id())) {
            // ensure not binding being parsed as type accidently
            if (t->get_id() == IDENTIFIER && lexer.peek_token(1)->get_id() == EQUAL) {
                break;
            }

            ::std::unique_ptr<AST::Type> type = parse_type();
            if (type == NULL) {
                // not necessarily an error
                break;
            }

            type_args.push_back(::std::move(type));

            // if next token isn't comma, then it must be end of list
            if (lexer.peek_token()->get_id() != COMMA) {
                break;
            }
            // skip comma
            lexer.skip_token();

            t = lexer.peek_token();
        }

        // try to parse bindings third
        ::std::vector<AST::GenericArgsBinding> binding_args;

        // TODO: think of better control structure
        t = lexer.peek_token();
        while (!is_right_angle_tok(t->get_id())) {
            AST::GenericArgsBinding binding = parse_generic_args_binding();
            if (binding.is_error()) {
                // not necessarily an error
                break;
            }

            binding_args.push_back(::std::move(binding));

            // if next token isn't comma, then it must be end of list
            if (lexer.peek_token()->get_id() != COMMA) {
                break;
            }
            // skip comma
            lexer.skip_token();

            t = lexer.peek_token();
        }

        // skip any trailing commas
        if (lexer.peek_token()->get_id() == COMMA) {
            lexer.skip_token();
        }

        if (!skip_generics_right_angle()) {
            return AST::GenericArgs::create_empty();
        }

        return AST::GenericArgs(
          ::std::move(lifetime_args), ::std::move(type_args), ::std::move(binding_args), locus);
    }

    // Parses a binding in a generic args path segment.
    AST::GenericArgsBinding Parser::parse_generic_args_binding() {
        const_TokenPtr ident_tok = lexer.peek_token();
        if (ident_tok->get_id() != IDENTIFIER) {
            // allow non error-inducing use
            // skip somewhere?
            return AST::GenericArgsBinding::create_error();
        }
        lexer.skip_token();
        Identifier ident = ident_tok->get_str();

        if (!skip_token(EQUAL)) {
            // skip after somewhere?
            return AST::GenericArgsBinding::create_error();
        }

        // parse type (required)
        ::std::unique_ptr<AST::Type> type = parse_type();
        if (type == NULL) {
            // skip somewhere?
            return AST::GenericArgsBinding::create_error();
        }

        return AST::GenericArgsBinding(::std::move(ident), ::std::move(type), ident_tok->get_locus());
    }

    /* Parses a single type path segment (not including opening scope resolution, but includes any
     * internal ones). Includes generic args or type path functions too. */
    ::std::unique_ptr<AST::TypePathSegment> Parser::parse_type_path_segment() {
        location_t locus = lexer.peek_token()->get_locus();
        // parse ident segment part
        AST::PathIdentSegment ident_segment = parse_path_ident_segment();
        if (ident_segment.is_error()) {
            // not necessarily an error
            return NULL;
        }

        // lookahead to determine if variants exist - only consume scope resolution then
        bool has_separating_scope_resolution = false;
        const_TokenPtr next = lexer.peek_token(1);
        if (lexer.peek_token()->get_id() == SCOPE_RESOLUTION
            && (next->get_id() == LEFT_ANGLE || next->get_id() == LEFT_PAREN)) {
            has_separating_scope_resolution = true;
            lexer.skip_token();
        }

        // branch into variants on next token
        const_TokenPtr t = lexer.peek_token();
        switch (t->get_id()) {
            case LEFT_ANGLE: {
                // parse generic args
                AST::GenericArgs generic_args = parse_path_generic_args();

                return ::std::unique_ptr<AST::TypePathSegmentGeneric>(
                  new AST::TypePathSegmentGeneric(::std::move(ident_segment),
                    has_separating_scope_resolution, ::std::move(generic_args), locus));
            }
            case LEFT_PAREN: {
                // parse type path function
                AST::TypePathFunction type_path_function = parse_type_path_function();

                if (type_path_function.is_error()) {
                    // skip after somewhere?
                    return NULL;
                }

                return ::std::unique_ptr<AST::TypePathSegmentFunction>(
                  new AST::TypePathSegmentFunction(::std::move(ident_segment),
                    has_separating_scope_resolution, ::std::move(type_path_function), locus));
            }
            default:
                // neither of them
                return ::std::unique_ptr<AST::TypePathSegment>(new AST::TypePathSegment(
                  ::std::move(ident_segment), has_separating_scope_resolution, locus));
        }
        gcc_unreachable();
    }

    // Parses a function call representation inside a type path.
    AST::TypePathFunction Parser::parse_type_path_function() {
        if (!skip_token(LEFT_PAREN)) {
            // skip somewhere?
            return AST::TypePathFunction::create_error();
        }

        // parse function inputs
        ::std::vector< ::std::unique_ptr<AST::Type> > inputs;

        // TODO: think of better control structure
        while (true) {
            ::std::unique_ptr<AST::Type> type = parse_type();
            if (type == NULL) {
                // not necessarily an error
                break;
            }

            inputs.push_back(::std::move(type));

            // skip commas, including trailing commas
            if (lexer.peek_token()->get_id() != COMMA) {
                break;
            } else {
                lexer.skip_token();
            }
        }

        if (!skip_token(RIGHT_PAREN)) {
            // skip somewhere?
            return AST::TypePathFunction::create_error();
        }

        // parse optional return type
        ::std::unique_ptr<AST::Type> return_type = parse_function_return_type();

        return AST::TypePathFunction(::std::move(inputs), ::std::move(return_type));
    }

    // Parses a path inside an expression that allows generic arguments.
    AST::PathInExpression Parser::parse_path_in_expression() {
        location_t locus = UNKNOWN_LOCATION;
        bool has_opening_scope_resolution = false;
        if (lexer.peek_token()->get_id() == SCOPE_RESOLUTION) {
            has_opening_scope_resolution = true;

            locus = lexer.peek_token()->get_locus();

            lexer.skip_token();
        }

        // create segment vector
        ::std::vector<AST::PathExprSegment> segments;

        if (locus == UNKNOWN_LOCATION) {
            locus = lexer.peek_token()->get_locus();
        }

        // parse required initial segment
        AST::PathExprSegment initial_segment = parse_path_expr_segment();
        if (initial_segment.is_error()) {
            // skip after somewhere?
            // don't necessarily throw error but yeah
            return AST::PathInExpression::create_error();
        }
        segments.push_back(::std::move(initial_segment));

        // parse optional segments (as long as scope resolution operator exists)
        const_TokenPtr t = lexer.peek_token();
        while (t->get_id() == SCOPE_RESOLUTION) {
            // skip scope resolution operator
            lexer.skip_token();

            // parse the actual segment - it is an error if it doesn't exist now
            AST::PathExprSegment segment = parse_path_expr_segment();
            if (segment.is_error()) {
                // skip after somewhere?
                error_at(t->get_locus(), "could not parse path expression segment");
                return AST::PathInExpression::create_error();
            }

            segments.push_back(::std::move(segment));

            t = lexer.peek_token();
        }

        return AST::PathInExpression(::std::move(segments), locus, has_opening_scope_resolution,
          ::std::vector<AST::Attribute>());
    }

    // Parses a single path in expression path segment (including generic arguments).
    AST::PathExprSegment Parser::parse_path_expr_segment() {
        location_t locus = lexer.peek_token()->get_locus();
        // parse ident segment
        AST::PathIdentSegment ident = parse_path_ident_segment();
        if (ident.is_error()) {
            // not necessarily an error?
            return AST::PathExprSegment::create_error();
        }

        // parse generic args (and turbofish), if they exist
        /* use lookahead to determine if they actually exist (don't want to accidently parse over
         * next ident segment) */
        if (lexer.peek_token()->get_id() == SCOPE_RESOLUTION
            && lexer.peek_token(1)->get_id() == LEFT_ANGLE) {
            // skip scope resolution
            lexer.skip_token();

            AST::GenericArgs generic_args = parse_path_generic_args();

            return AST::PathExprSegment(::std::move(ident), locus, ::std::move(generic_args));
        }

        // return a generic parameter-less expr segment if not found
        return AST::PathExprSegment(::std::move(ident), locus);
    }

    // Parses a fully qualified path in expression (i.e. a pattern). FIXME does not parse outer attrs.
    AST::QualifiedPathInExpression Parser::parse_qualified_path_in_expression(bool pratt_parse) {
        /* Note: the Rust grammar is defined in such a way that it is impossible to determine whether
         * a prospective qualified path is a QualifiedPathInExpression or QualifiedPathInType in all
         * cases by the rules themselves (the only possible difference is a TypePathSegment with
         * function, and lookahead to find this is too difficult). However, as this is a pattern and
         * QualifiedPathInType is a type, I believe it that their construction will not be confused
         * (due to rules regarding patterns vs types).
         * As such, this function will not attempt to minimise errors created by their confusion. */

        // parse the qualified path type (required)
        AST::QualifiedPathType qual_path_type = parse_qualified_path_type(pratt_parse);
        if (qual_path_type.is_error()) {
            // TODO: should this create a parse error?
            return AST::QualifiedPathInExpression::create_error();
        }
        location_t locus = qual_path_type.get_locus();

        // parse path segments
        ::std::vector<AST::PathExprSegment> segments;

        // parse initial required segment
        if (!expect_token(SCOPE_RESOLUTION)) {
            // skip after somewhere?

            return AST::QualifiedPathInExpression::create_error();
        }
        AST::PathExprSegment initial_segment = parse_path_expr_segment();
        if (initial_segment.is_error()) {
            // skip after somewhere?
            error_at(lexer.peek_token()->get_locus(),
              "required initial path expression segment in "
              "qualified path in expression could not be parsed");
            return AST::QualifiedPathInExpression::create_error();
        }
        segments.push_back(::std::move(initial_segment));

        // parse optional segments (as long as scope resolution operator exists)
        const_TokenPtr t = lexer.peek_token();
        while (t->get_id() == SCOPE_RESOLUTION) {
            // skip scope resolution operator
            lexer.skip_token();

            // parse the actual segment - it is an error if it doesn't exist now
            AST::PathExprSegment segment = parse_path_expr_segment();
            if (segment.is_error()) {
                // skip after somewhere?
                error_at(t->get_locus(),
                  "could not parse path expression segment in qualified path in expression");
                return AST::QualifiedPathInExpression::create_error();
            }

            segments.push_back(::std::move(segment));

            t = lexer.peek_token();
        }

        // FIXME: outer attr parsing
        return AST::QualifiedPathInExpression(
          ::std::move(qual_path_type), ::std::move(segments), locus, ::std::vector<AST::Attribute>());
    }

    // Parses the type syntactical construction at the start of a qualified path.
    AST::QualifiedPathType Parser::parse_qualified_path_type(bool pratt_parse) {
        location_t locus = UNKNOWN_LOCATION;
        // TODO: should this actually be error? is there anywhere where this could be valid?
        if (!pratt_parse) {
            locus = lexer.peek_token()->get_locus();
            if (!skip_token(LEFT_ANGLE)) {
                // skip after somewhere?
                return AST::QualifiedPathType::create_error();
            }
        } else {
            // move back by 1 if pratt parsing due to skipping '<'
            locus = lexer.peek_token()->get_locus() - 1;
        }

        // parse type (required)
        ::std::unique_ptr<AST::Type> type = parse_type();
        if (type == NULL) {
            error_at(lexer.peek_token()->get_locus(), "could not parse type in qualified path type");
            // skip somewhere?
            return AST::QualifiedPathType::create_error();
        }

        // parse optional as clause
        AST::TypePath as_type_path = AST::TypePath::create_error();
        if (lexer.peek_token()->get_id() == AS) {
            lexer.skip_token();

            // parse type path, which is required now
            as_type_path = parse_type_path();
            if (as_type_path.is_error()) {
                error_at(lexer.peek_token()->get_locus(),
                  "could not parse type path in as clause in qualified path type");
                // skip somewhere?
                return AST::QualifiedPathType::create_error();
            }
        }

        // NOTE: should actually be a right-angle token, so skip_generics_right_angle shouldn't be
        // required
        if (!skip_token(RIGHT_ANGLE)) {
            // skip after somewhere?
            return AST::QualifiedPathType::create_error();
        }

        return AST::QualifiedPathType(::std::move(type), locus, ::std::move(as_type_path));
    }

    // Parses a fully qualified path in type (i.e. a type).
    AST::QualifiedPathInType Parser::parse_qualified_path_in_type() {
        location_t locus = lexer.peek_token()->get_locus();
        // parse the qualified path type (required)
        AST::QualifiedPathType qual_path_type = parse_qualified_path_type();
        if (qual_path_type.is_error()) {
            // TODO: should this create a parse error?
            return AST::QualifiedPathInType::create_error();
        }

        // parse path segments
        ::std::vector< ::std::unique_ptr<AST::TypePathSegment> > segments;

        // parse initial required segment
        if (!expect_token(SCOPE_RESOLUTION)) {
            // skip after somewhere?

            return AST::QualifiedPathInType::create_error();
        }
        ::std::unique_ptr<AST::TypePathSegment> initial_segment = parse_type_path_segment();
        if (initial_segment == NULL) {
            // skip after somewhere?
            error_at(lexer.peek_token()->get_locus(),
              "required initial type path segment in qualified path in type could not be parsed");
            return AST::QualifiedPathInType::create_error();
        }
        segments.push_back(::std::move(initial_segment));

        // parse optional segments (as long as scope resolution operator exists)
        const_TokenPtr t = lexer.peek_token();
        while (t->get_id() == SCOPE_RESOLUTION) {
            // skip scope resolution operator
            lexer.skip_token();

            // parse the actual segment - it is an error if it doesn't exist now
            ::std::unique_ptr<AST::TypePathSegment> segment = parse_type_path_segment();
            if (segment == NULL) {
                // skip after somewhere?
                error_at(
                  t->get_locus(), "could not parse type path segment in qualified path in type");
                return AST::QualifiedPathInType::create_error();
            }

            segments.push_back(::std::move(segment));

            t = lexer.peek_token();
        }

        return AST::QualifiedPathInType(::std::move(qual_path_type), ::std::move(segments), locus);
    }

    // Parses a self param. Also handles self param not existing.
    AST::SelfParam Parser::parse_self_param() {
        bool has_reference = false;
        AST::Lifetime lifetime = AST::Lifetime::error();

        location_t locus = lexer.peek_token()->get_locus();

        // test if self is a reference parameter
        if (lexer.peek_token()->get_id() == AMP) {
            has_reference = true;
            lexer.skip_token();

            // now test whether it has a lifetime
            if (lexer.peek_token()->get_id() == LIFETIME) {
                lifetime = parse_lifetime();

                // something went wrong somehow
                if (lifetime.is_error()) {
                    error_at(
                      lexer.peek_token()->get_locus(), "failed to parse lifetime in self param");
                    // skip after somewhere?
                    return AST::SelfParam::create_error();
                }
            }
        }

        // test for mut
        bool has_mut = false;
        if (lexer.peek_token()->get_id() == MUT) {
            has_mut = true;
            lexer.skip_token();
        }

        // skip self token
        const_TokenPtr self_tok = lexer.peek_token();
        if (self_tok->get_id() != SELF) {
            // skip after somewhere?
            return AST::SelfParam::create_error();
        }
        lexer.skip_token();

        // parse optional type
        ::std::unique_ptr<AST::Type> type = NULL;
        if (lexer.peek_token()->get_id() == COLON) {
            lexer.skip_token();

            // type is now required
            type = parse_type();
            if (type == NULL) {
                error_at(lexer.peek_token()->get_locus(), "could not parse type in self param");
                // skip after somewhere?
                return AST::SelfParam::create_error();
            }
        }

        // ensure that cannot have both type and reference
        if (type != NULL && has_reference) {
            error_at(lexer.peek_token()->get_locus(),
              "cannot have both a reference and a type specified in a self param");
            // skip after somewhere?
            return AST::SelfParam::create_error();
        }

        if (has_reference) {
            return AST::SelfParam(::std::move(lifetime), has_mut, locus);
        } else {
            // note that type may be NULL here and that's fine
            return AST::SelfParam(::std::move(type), has_mut, locus);
        }
    }

    /* Parses a method. Note that this function is probably useless because using lookahead to
     * determine whether a function is a method is a PITA (maybe not even doable), so most places
     * probably parse a "function or method" and then resolve it into whatever it is afterward. As
     * such, this is only here for algorithmically defining the grammar rule. */
    AST::Method Parser::parse_method() {
        location_t locus = lexer.peek_token()->get_locus();
        // Note: as a result of the above, this will not attempt to disambiguate a function
        // parse qualifiers
        AST::FunctionQualifiers qualifiers = parse_function_qualifiers();

        skip_token(FN_TOK);

        const_TokenPtr ident_tok = expect_token(IDENTIFIER);
        if (ident_tok == NULL) {
            skip_after_next_block();
            return AST::Method::create_error();
        }
        Identifier method_name = ident_tok->get_str();

        // parse generic params - if exist
        ::std::vector< ::std::unique_ptr<AST::GenericParam> > generic_params
          = parse_generic_params_in_angles();

        if (!skip_token(LEFT_PAREN)) {
            error_at(lexer.peek_token()->get_locus(),
              "method missing opening parentheses before parameter list");
            skip_after_next_block();
            return AST::Method::create_error();
        }

        // parse self param
        AST::SelfParam self_param = parse_self_param();
        if (self_param.is_error()) {
            error_at(lexer.peek_token()->get_locus(), "could not parse self param in method");
            skip_after_next_block();
            return AST::Method::create_error();
        }

        // skip comma if it exists
        if (lexer.peek_token()->get_id() == COMMA) {
            lexer.skip_token();
        }

        // parse function parameters
        ::std::vector<AST::FunctionParam> function_params = parse_function_params();

        if (!skip_token(RIGHT_PAREN)) {
            error_at(lexer.peek_token()->get_locus(),
              "method declaration missing closing parentheses after parameter list");
            skip_after_next_block();
            return AST::Method::create_error();
        }

        // parse function return type - if exists
        ::std::unique_ptr<AST::Type> return_type = parse_function_return_type();

        // parse where clause - if exists
        AST::WhereClause where_clause = parse_where_clause();

        // parse block expression
        ::std::unique_ptr<AST::BlockExpr> block_expr = parse_block_expr();
        if (block_expr == NULL) {
            error_at(lexer.peek_token()->get_locus(), "method declaration missing block expression");
            skip_after_end_block();
            return AST::Method::create_error();
        }

        // does not parse visibility, but this method isn't used, so doesn't matter
        return AST::Method(::std::move(method_name), ::std::move(qualifiers),
          ::std::move(generic_params), ::std::move(self_param), ::std::move(function_params),
          ::std::move(return_type), ::std::move(where_clause), ::std::move(block_expr),
          AST::Visibility::create_error(), ::std::vector<AST::Attribute>(), locus);
    }

    // Parses an expression statement (disambiguates to expression with or without block statement).
    ::std::unique_ptr<AST::ExprStmt> Parser::parse_expr_stmt(
      ::std::vector<AST::Attribute> outer_attrs) {
        /* potential thoughts - define new virtual method "has_block()" on expr. parse expr and then
         * determine whether semicolon is needed as a result of this method.
         * but then this would require dynamic_cast, which is not allowed. */

        /* okay new thought - big switch to disambiguate exprs with blocks - either block expr,
         * async block expr, unsafe block expr, loop expr, if expr, if let expr, or match expr. So
         * all others are exprs without block. */
        /* new thought: possible initial tokens: 'loop', 'while', 'for', lifetime (and then ':' and
         * then loop), 'if', 'match', '{', 'async', 'unsafe' (and then '{')). This seems to have no
         * ambiguity. */

        const_TokenPtr t = lexer.peek_token();
        /* TODO: should the switch just directly call the individual parse methods rather than adding
         * another layer of indirection with parse_expr_stmt_with_block()? */
        switch (t->get_id()) {
            case LOOP:
            case WHILE:
            case FOR:
            case IF:
            case MATCH_TOK:
            case LEFT_CURLY:
            case ASYNC:
                // expression with block
                return parse_expr_stmt_with_block(::std::move(outer_attrs));
            case LIFETIME: {
                /* FIXME: are there any expressions without blocks that can have lifetime as their
                 * first token? Or is loop expr the only one? */
                // safe side for now:
                if (lexer.peek_token(1)->get_id() == COLON && lexer.peek_token(2)->get_id() == LOOP) {
                    return parse_expr_stmt_with_block(::std::move(outer_attrs));
                } else {
                    return parse_expr_stmt_without_block(::std::move(outer_attrs));
                }
            }
            case UNSAFE: {
                /* FIXME: are there any expressions without blocks that can have unsafe as their
                 * first token? Or is unsafe the only one? */
                // safe side for now
                if (lexer.peek_token(1)->get_id() == LEFT_CURLY) {
                    return parse_expr_stmt_with_block(::std::move(outer_attrs));
                } else {
                    return parse_expr_stmt_without_block(::std::move(outer_attrs));
                }
            }
            default:
                // not a parse expr with block, so must be expr without block
                /* TODO: if possible, be more selective about possible expr without block initial
                 * tokens in order to prevent more syntactical errors at parse time. */
                return parse_expr_stmt_without_block(::std::move(outer_attrs));
        }
    }

    // Parses a expression statement containing an expression with block. Disambiguates internally.
    ::std::unique_ptr<AST::ExprStmtWithBlock> Parser::parse_expr_stmt_with_block(
      ::std::vector<AST::Attribute> outer_attrs) {
        ::std::unique_ptr<AST::ExprWithBlock> expr_parsed = NULL;

        const_TokenPtr t = lexer.peek_token();
        switch (t->get_id()) {
            case IF:
                // if or if let, so more lookahead to find out
                if (lexer.peek_token(1)->get_id() == LET) {
                    // if let expr
                    expr_parsed = parse_if_let_expr(::std::move(outer_attrs));
                    break;
                } else {
                    // if expr
                    expr_parsed = parse_if_expr(::std::move(outer_attrs));
                    break;
                }
            case LOOP:
                // infinite loop
                expr_parsed = parse_loop_expr(::std::move(outer_attrs));
                break;
            case FOR:
                // "for" iterator loop
                expr_parsed = parse_for_loop_expr(::std::move(outer_attrs));
                break;
            case WHILE: {
                // while or while let, so more lookahead to find out
                if (lexer.peek_token()->get_id() == LET) {
                    // while let loop expr
                    expr_parsed = parse_while_let_loop_expr(::std::move(outer_attrs));
                    break;
                } else {
                    // while loop expr
                    expr_parsed = parse_while_loop_expr(::std::move(outer_attrs));
                    break;
                }
            }
            case MATCH_TOK:
                // match expression
                expr_parsed = parse_match_expr(::std::move(outer_attrs));
                break;
            case LEFT_CURLY:
                // block expression
                expr_parsed = parse_block_expr(::std::move(outer_attrs));
                break;
            case ASYNC:
                // async block expression
                expr_parsed = parse_async_block_expr(::std::move(outer_attrs));
                break;
            case UNSAFE:
                // unsafe block expression
                expr_parsed = parse_unsafe_block_expr(::std::move(outer_attrs));
                break;
            case LIFETIME:
                // some kind of loop expr (with loop label)
                expr_parsed = parse_labelled_loop_expr(::std::move(outer_attrs));
                break;
            default:
                error_at(t->get_locus(),
                  "could not recognise expr beginning with '%s' as an expr with block in parsing "
                  "expr statement.",
                  t->get_token_description());
                skip_after_next_block();
                return NULL;
        }

        // ensure expr parsed exists
        if (expr_parsed == NULL) {
            error_at(t->get_locus(), "failed to parse expr with block in parsing expr statement");
            skip_after_end_block();
            return NULL;
        }

        // return expr stmt created from expr
        return ::std::unique_ptr<AST::ExprStmtWithBlock>(
          new AST::ExprStmtWithBlock(::std::move(expr_parsed), t->get_locus()));
    }

    // Parses an expression statement containing an expression without block. Disambiguates further.
    ::std::unique_ptr<AST::ExprStmtWithoutBlock> Parser::parse_expr_stmt_without_block(
      ::std::vector<AST::Attribute> outer_attrs) {
        // TODO: maybe move more logic for expr without block in here for better error handling

        // try to parse expr without block
        /*AST::ExprWithoutBlock* expr = NULL;
        expr = parse_expr_without_block(::std::move(outer_attrs));*/
        // HACK: parse expression instead of expression without block, due to Pratt parsing issues
        ::std::unique_ptr<AST::Expr> expr = NULL;
        location_t locus = lexer.peek_token()->get_locus();
        expr = parse_expr(::std::move(outer_attrs));
        if (expr == NULL) {
            // expr is required, error
            error_at(lexer.peek_token()->get_locus(),
              "failed to parse expr without block in expr statement");
            skip_after_semicolon();
            return NULL;
        }

        // skip semicolon at end that is required
        if (!skip_token(SEMICOLON)) {
            // skip somewhere?
            return NULL;
        }

        return ::std::unique_ptr<AST::ExprStmtWithoutBlock>(
          new AST::ExprStmtWithoutBlock(::std::move(expr), locus));
    }

    // Parses an expression without a block associated with it (further disambiguates).
    ::std::unique_ptr<AST::ExprWithoutBlock> Parser::parse_expr_without_block(
      ::std::vector<AST::Attribute> outer_attrs) {
        /* Notes on types of expr without block:
         *  - literal expr          tokens that are literals
         *  - path expr             path_in_expr or qual_path_in_expr
         *  - operator expr         many different types
         *     unary:
         *      borrow expr         ( '&' | '&&' ) 'mut'? expr
         *      dereference expr    '*' expr
         *      error propagation   expr '?'
         *      negation            '-' expr
         *      not                 '!' expr
         *     binary: all start with expr
         *  - grouped/paren expr    '(' inner_attributes expr ')'
         *  - array expr            '[' inner_attributes array_elems? ']'
         *  - await expr            expr '.' 'await'
         *  - (array/slice) index expr  expr '[' expr ']'
         *  - tuple expr            '(' inner_attributes tuple_elems? ')'
         *      note that a single elem tuple is distinguished from a grouped expr by a trailing
         *      comma, i.e. a grouped expr is preferred over a tuple expr
         *  - tuple index expr      expr '.' tuple_index
         *  - struct expr           path_in_expr (and optional other stuff)
         *  - enum variant expr     path_in_expr (and optional other stuff)
         *      this means that there is no syntactic difference between an enum variant and a struct
         *      - only name resolution can tell the difference. Thus, maybe rework AST to take this
         *      into account ("struct or enum" nodes?)
         *  - (function) call expr  expr '(' call_params? ')'
         *  - method call expr      expr '.' path_expr_segment '(' call_params? ')'
         *  - field expr            expr '.' identifier
         *      note that method call expr is preferred, i.e. field expr must not be followed by
         *      parenthesised expression sequence.
         *  - closure expr          'move'? ( '||' | '|' closure_params? '|' ) ( expr | '->'
         * type_no_bounds block_expr )
         *  - continue expr         'continue' labelled_lifetime?
         *  - break expr            'break' labelled_lifetime? expr?
         *  - range expr            many different types but all involve '..' or '..='
         *  - return expr           'return' as 1st tok
         *  - macro invocation      identifier then :: or identifier then ! (simple_path '!')
         *
         * any that have rules beginning with 'expr' should probably be pratt-parsed, with parsing
         * type to use determined by token AND lookahead. */

        // ok well at least can do easy ones
        const_TokenPtr t = lexer.peek_token();
        switch (t->get_id()) {
            case RETURN_TOK:
                // return expr
                return parse_return_expr(::std::move(outer_attrs));
            case BREAK:
                // break expr
                return parse_break_expr(::std::move(outer_attrs));
            case CONTINUE:
                // continue expr
                return parse_continue_expr(::std::move(outer_attrs));
            case MOVE:
                // closure expr (though not all closure exprs require this)
                return parse_closure_expr(::std::move(outer_attrs));
            case LEFT_SQUARE:
                // array expr (creation, not index)
                return parse_array_expr(::std::move(outer_attrs));
            case LEFT_PAREN:
                /* either grouped expr or tuple expr - depends on whether there is a comma inside the
                 * parentheses - if so, tuple expr, otherwise, grouped expr. */
                return parse_grouped_or_tuple_expr(::std::move(outer_attrs));
            default: {
                // HACK: piggyback on pratt parsed expr and abuse polymorphism to essentially downcast

                // DEBUG
                fprintf(stderr, "about to parse expr (in expr without block method)\n");

                ::std::unique_ptr<AST::Expr> expr = parse_expr(::std::move(outer_attrs));

                // DEBUG
                fprintf(stderr, "successfully parsed expr (in expr without block method)\n");

                if (expr == NULL) {
                    error_at(t->get_locus(), "failed to parse expression for expression without "
                                             "block (pratt-parsed expression is null)");
                    return NULL;
                }

                ::std::unique_ptr<AST::ExprWithoutBlock> expr_without_block(
                  expr->as_expr_without_block());
                // THIS IS THE CAUSE OF THE SEGFAULT

                // DEBUG
                fprintf(stderr, "expr to expr without block conversion didn't error\n");

                if (expr_without_block != NULL) {

                    // DEBUG
                    fprintf(
                      stderr, "expr to expr without block conversion was successful; returning\n");

                    return expr_without_block;
                } else {
                    error_at(t->get_locus(), "converted expr without block is null");
                    return NULL;
                }
            }
        }
    }

    // Parses a block expression, including the curly braces at start and end.
    ::std::unique_ptr<AST::BlockExpr> Parser::parse_block_expr(
      ::std::vector<AST::Attribute> outer_attrs, bool pratt_parse) {
        location_t locus = UNKNOWN_LOCATION;
        if (!pratt_parse) {
            locus = lexer.peek_token()->get_locus();
            if (!skip_token(LEFT_CURLY)) {
                skip_after_end_block();
                return NULL;
            }
        } else {
            locus = lexer.peek_token()->get_locus() - 1;
        }

        ::std::vector<AST::Attribute> inner_attrs = parse_inner_attributes();

        // parse statements and expression
        ::std::vector< ::std::unique_ptr<AST::Stmt> > stmts;
        ::std::unique_ptr<AST::ExprWithoutBlock> expr = NULL;

        const_TokenPtr t = lexer.peek_token();
        while (t->get_id() != RIGHT_CURLY) {
            ExprOrStmt expr_or_stmt = parse_stmt_or_expr_without_block();
            if (expr_or_stmt.is_error()) {
                error_at(t->get_locus(),
                  "failed to parse statement or expression without block in block expression");
                return NULL;
            }

            if (expr_or_stmt.stmt != NULL) {
                // FIXME: determine if this move works
                stmts.push_back(::std::move(expr_or_stmt.stmt));
            } else {
                // assign to expression and end parsing inside
                expr = ::std::move(expr_or_stmt.expr);
                break;
            }

            t = lexer.peek_token();
        }

        if (!skip_token(RIGHT_CURLY)) {
            error_at(t->get_locus(), "error may be from having an expression (as opposed to "
                                     "statement) in the body of the function but not last");
            skip_after_end_block();
            return NULL;
        }

        // ensure that there is at least either a statement or an expr
        /*if (stmts.empty() && expr == NULL) {
            error_at(lexer.peek_token()->get_id(),
              "block expression requires statements or an expression without block - found neither");
            skip_after_end_block();
            return NULL;
        }*/
        // grammar allows for empty block expressions

        return ::std::unique_ptr<AST::BlockExpr>(new AST::BlockExpr(::std::move(stmts),
          ::std::move(expr), ::std::move(inner_attrs), ::std::move(outer_attrs), locus));
    }

    // Parses a "grouped" expression (expression in parentheses), used to control precedence.
    ::std::unique_ptr<AST::GroupedExpr> Parser::parse_grouped_expr(
      ::std::vector<AST::Attribute> outer_attrs) {
        location_t locus = lexer.peek_token()->get_locus();
        skip_token(LEFT_PAREN);

        ::std::vector<AST::Attribute> inner_attrs = parse_inner_attributes();

        // parse required expr inside parentheses
        ::std::unique_ptr<AST::Expr> expr_in_parens = parse_expr();
        if (expr_in_parens == NULL) {
            // skip after somewhere?
            // error?
            return NULL;
        }

        if (!skip_token(RIGHT_PAREN)) {
            // skip after somewhere?
            return NULL;
        }

        return ::std::unique_ptr<AST::GroupedExpr>(new AST::GroupedExpr(
          ::std::move(expr_in_parens), ::std::move(inner_attrs), ::std::move(outer_attrs), locus));
    }

    // Parses a closure expression (closure definition).
    ::std::unique_ptr<AST::ClosureExpr> Parser::parse_closure_expr(
      ::std::vector<AST::Attribute> outer_attrs) {
        location_t locus = lexer.peek_token()->get_locus();
        // detect optional "move"
        bool has_move = false;
        if (lexer.peek_token()->get_id() == MOVE) {
            lexer.skip_token();
            has_move = true;
        }

        // handle parameter list
        ::std::vector<AST::ClosureParam> params;

        const_TokenPtr t = lexer.peek_token();
        switch (t->get_id()) {
            case OR:
                // skip token, no parameters
                lexer.skip_token();
                break;
            case PIPE:
                // actually may have parameters
                lexer.skip_token();

                while (t->get_id() != PIPE) {
                    AST::ClosureParam param = parse_closure_param();
                    if (param.is_error()) {
                        // TODO is this really an error?
                        error_at(t->get_locus(), "could not parse closure param");
                        break;
                    }
                    params.push_back(::std::move(param));

                    if (lexer.peek_token()->get_id() != COMMA) {
                        // not an error but means param list is done
                        break;
                    }
                    // skip comma
                    lexer.skip_token();

                    t = lexer.peek_token();
                }
                break;
            default:
                error_at(t->get_locus(),
                  "unexpected token '%s' in closure expression - expected '|' or '||'",
                  t->get_token_description());
                // skip somewhere?
                return NULL;
        }

        // again branch based on next token
        t = lexer.peek_token();
        if (t->get_id() == RETURN_TYPE) {
            // must be return type closure with block expr

            // skip "return type" token
            lexer.skip_token();

            // parse actual type, which is required
            ::std::unique_ptr<AST::TypeNoBounds> type = parse_type_no_bounds();
            if (type == NULL) {
                // error
                error_at(t->get_locus(), "failed to parse type for closure");
                // skip somewhere?
                return NULL;
            }

            // parse block expr, which is required
            ::std::unique_ptr<AST::BlockExpr> block = parse_block_expr();
            if (block == NULL) {
                // error
                error_at(lexer.peek_token()->get_locus(), "failed to parse block expr in closure");
                // skip somewhere?
                return NULL;
            }

            return ::std::unique_ptr<AST::ClosureExprInnerTyped>(
              new AST::ClosureExprInnerTyped(::std::move(type), ::std::move(block),
                ::std::move(params), locus, has_move, ::std::move(outer_attrs)));
        } else {
            // must be expr-only closure

            // parse expr, which is required
            ::std::unique_ptr<AST::Expr> expr = parse_expr();
            if (expr == NULL) {
                error_at(t->get_locus(), "failed to parse expression in closure");
                // skip somewhere?
                return NULL;
            }

            return ::std::unique_ptr<AST::ClosureExprInner>(new AST::ClosureExprInner(
              ::std::move(expr), ::std::move(params), locus, has_move, ::std::move(outer_attrs)));
        }
    }

    // Parses a literal token (to literal expression).
    ::std::unique_ptr<AST::LiteralExpr> Parser::parse_literal_expr(
      ::std::vector<AST::Attribute> outer_attrs) {
        // TODO: change if literal representation in lexer changes

        ::std::string literal_value;
        AST::Literal::LitType type = AST::Literal::STRING;

        // branch based on token
        const_TokenPtr t = lexer.peek_token();
        switch (t->get_id()) {
            case CHAR_LITERAL:
                type = AST::Literal::CHAR;
                literal_value = t->get_str();
                lexer.skip_token();
                break;
            case STRING_LITERAL:
                type = AST::Literal::STRING;
                literal_value = t->get_str();
                lexer.skip_token();
                break;
            // case RAW_STRING_LITERAL:
            // put here if lexer changes to have these
            case BYTE_CHAR_LITERAL:
                type = AST::Literal::BYTE;
                literal_value = t->get_str();
                lexer.skip_token();
                break;
            case BYTE_STRING_LITERAL:
                type = AST::Literal::BYTE_STRING;
                literal_value = t->get_str();
                lexer.skip_token();
                break;
            // case RAW_BYTE_STRING_LITERAL:
            case INT_LITERAL:
                type = AST::Literal::INT;
                literal_value = t->get_str();
                lexer.skip_token();
                break;
            case FLOAT_LITERAL:
                type = AST::Literal::FLOAT;
                literal_value = t->get_str();
                lexer.skip_token();
                break;
            // case BOOL_LITERAL
            // use true and false keywords rather than "bool literal" Rust terminology
            case TRUE_LITERAL:
                type = AST::Literal::BOOL;
                literal_value = ::std::string("true");
                lexer.skip_token();
                break;
            case FALSE_LITERAL:
                type = AST::Literal::BOOL;
                literal_value = ::std::string("false");
                lexer.skip_token();
                break;
            default:
                // error - cannot be a literal expr
                error_at(t->get_locus(), "unexpected token '%s' when parsing literal expression",
                  t->get_token_description());
                // skip?
                return NULL;
        }

        // create literal based on stuff in switch
        return ::std::unique_ptr<AST::LiteralExpr>(new AST::LiteralExpr(
          ::std::move(literal_value), ::std::move(type), t->get_locus(), ::std::move(outer_attrs)));
    }

    // Parses a return expression (including any expression to return).
    ::std::unique_ptr<AST::ReturnExpr> Parser::parse_return_expr(
      ::std::vector<AST::Attribute> outer_attrs, bool pratt_parse) {
        location_t locus = UNKNOWN_LOCATION;
        if (!pratt_parse) {
            locus = lexer.peek_token()->get_locus();

            skip_token(RETURN_TOK);
        } else {
            // minus 7 chars for 6 in return and a space
            // or just TODO: pass in location data
            locus = lexer.peek_token()->get_locus() - 7;
        }

        // parse expression to return, if it exists
        ::std::unique_ptr<AST::Expr> returned_expr = parse_expr();
        // FIXME: ensure this doesn't ruin the middle of any expressions or anything

        return ::std::unique_ptr<AST::ReturnExpr>(
          new AST::ReturnExpr(locus, ::std::move(returned_expr), ::std::move(outer_attrs)));
    }

    // Parses a break expression (including any label to break to AND any return expression).
    ::std::unique_ptr<AST::BreakExpr> Parser::parse_break_expr(
      ::std::vector<AST::Attribute> outer_attrs, bool pratt_parse) {
        location_t locus = UNKNOWN_LOCATION;
        if (!pratt_parse) {
            locus = lexer.peek_token()->get_locus();

            skip_token(BREAK);
        } else {
            // minus 6 chars for 5 in return and a space
            // or just TODO: pass in location data
            locus = lexer.peek_token()->get_locus() - 6;
        }

        // parse label (lifetime) if it exists - create dummy first
        AST::Lifetime label = AST::Lifetime::error();
        if (lexer.peek_token()->get_id() == LIFETIME) {
            label = parse_lifetime();
        }

        // parse break return expression if it exists
        ::std::unique_ptr<AST::Expr> return_expr = parse_expr();

        return ::std::unique_ptr<AST::BreakExpr>(new AST::BreakExpr(
          locus, ::std::move(label), ::std::move(return_expr), ::std::move(outer_attrs)));
    }

    // Parses a continue expression (including any label to continue from).
    ::std::unique_ptr<AST::ContinueExpr> Parser::parse_continue_expr(
      ::std::vector<AST::Attribute> outer_attrs, bool pratt_parse) {
        location_t locus = UNKNOWN_LOCATION;
        if (!pratt_parse) {
            locus = lexer.peek_token()->get_locus();

            skip_token(CONTINUE);
        } else {
            // minus 9 chars for 8 in return and a space
            // or just TODO: pass in location data
            locus = lexer.peek_token()->get_locus() - 9;
        }

        // parse label (lifetime) if it exists - create dummy first
        AST::Lifetime label = AST::Lifetime::error();
        if (lexer.peek_token()->get_id() == LIFETIME) {
            label = parse_lifetime();
        }

        return ::std::unique_ptr<AST::ContinueExpr>(
          new AST::ContinueExpr(locus, ::std::move(label), ::std::move(outer_attrs)));
    }

    // Parses a loop label used in loop expressions.
    AST::LoopLabel Parser::parse_loop_label() {
        // parse lifetime - if doesn't exist, assume no label
        const_TokenPtr t = lexer.peek_token();
        if (t->get_id() != LIFETIME) {
            // not necessarily an error
            return AST::LoopLabel::error();
        }
        // FIXME: check for named lifetime requirement here? or check in semantic analysis phase?
        AST::Lifetime label = parse_lifetime();

        if (!skip_token(COLON)) {
            // skip somewhere?
            return AST::LoopLabel::error();
        }

        return AST::LoopLabel(::std::move(label), t->get_locus());
    }

    /* Parses an if expression of any kind, including with else, else if, else if let, and neither.
     * Note that any outer attributes will be ignored because if expressions don't support them. */
    ::std::unique_ptr<AST::IfExpr> Parser::parse_if_expr(
      ::std::vector<AST::Attribute> outer_attrs ATTRIBUTE_UNUSED) {
        // TODO: make having outer attributes an error?

        location_t locus = lexer.peek_token()->get_locus();
        skip_token(IF);

        // detect accidental if let
        if (lexer.peek_token()->get_id() == LET) {
            error_at(lexer.peek_token()->get_locus(),
              "if let expression probably exists, but is being parsed as an if expression. This may "
              "be a parser error.");
            // skip somewhere?
            return NULL;
        }

        // parse required condition expr - HACK to prevent struct expr from being parsed
        ParseRestrictions no_struct_expr;
        no_struct_expr.can_be_struct_expr = false;
        ::std::unique_ptr<AST::Expr> condition
          = parse_expr(::std::vector<AST::Attribute>(), no_struct_expr);
        if (condition == NULL) {
            error_at(lexer.peek_token()->get_locus(),
              "failed to parse condition expression in if expression");
            // skip somewhere?
            return NULL;
        }

        // parse required block expr
        ::std::unique_ptr<AST::BlockExpr> if_body = parse_block_expr();
        if (if_body == NULL) {
            error_at(lexer.peek_token()->get_locus(),
              "failed to parse if body block expression in if expression");
            // skip somewhere?
            return NULL;
        }

        // branch to parse end or else (and then else, else if, or else if let)
        if (lexer.peek_token()->get_id() != ELSE) {
            // single selection - end of if expression
            return ::std::unique_ptr<AST::IfExpr>(
              new AST::IfExpr(::std::move(condition), ::std::move(if_body), locus));
        } else {
            // double or multiple selection - branch on end, else if, or else if let

            // skip "else"
            lexer.skip_token();

            // branch on whether next token is '{' or 'if'
            const_TokenPtr t = lexer.peek_token();
            switch (t->get_id()) {
                case LEFT_CURLY: {
                    // double selection - else
                    // parse else block expr (required)
                    ::std::unique_ptr<AST::BlockExpr> else_body = parse_block_expr();
                    if (else_body == NULL) {
                        error_at(lexer.peek_token()->get_locus(),
                          "failed to parse else body block expression in if expression");
                        // skip somewhere?
                        return NULL;
                    }

                    return ::std::unique_ptr<AST::IfExprConseqElse>(new AST::IfExprConseqElse(
                      ::std::move(condition), ::std::move(if_body), ::std::move(else_body), locus));
                }
                case IF: {
                    // multiple selection - else if or else if let
                    // branch on whether next token is 'let' or not
                    if (lexer.peek_token(1)->get_id() == LET) {
                        // parse if let expr (required)
                        ::std::unique_ptr<AST::IfLetExpr> if_let_expr = parse_if_let_expr();
                        if (if_let_expr == NULL) {
                            error_at(lexer.peek_token()->get_locus(),
                              "failed to parse (else) if let expression after if expression");
                            // skip somewhere?
                            return NULL;
                        }

                        return ::std::unique_ptr<AST::IfExprConseqIfLet>(
                          new AST::IfExprConseqIfLet(::std::move(condition), ::std::move(if_body),
                            ::std::move(if_let_expr), locus));
                    } else {
                        // parse if expr (required)
                        ::std::unique_ptr<AST::IfExpr> if_expr = parse_if_expr();
                        if (if_expr == NULL) {
                            error_at(lexer.peek_token()->get_locus(),
                              "failed to parse (else) if expression after if expression");
                            // skip somewhere?
                            return NULL;
                        }

                        return ::std::unique_ptr<AST::IfExprConseqIf>(new AST::IfExprConseqIf(
                          ::std::move(condition), ::std::move(if_body), ::std::move(if_expr), locus));
                    }
                }
                default:
                    // error - invalid token
                    error_at(t->get_locus(), "unexpected token '%s' after else in if expression",
                      t->get_token_description());
                    // skip somewhere?
                    return NULL;
            }
        }
    }

    /* Parses an if let expression of any kind, including with else, else if, else if let, and none.
     * Note that any outer attributes will be ignored as if let expressions don't support them. */
    ::std::unique_ptr<AST::IfLetExpr> Parser::parse_if_let_expr(
      ::std::vector<AST::Attribute> outer_attrs ATTRIBUTE_UNUSED) {
        // TODO: make having outer attributes an error?

        location_t locus = lexer.peek_token()->get_locus();
        skip_token(IF);

        // detect accidental if expr parsed as if let expr
        if (lexer.peek_token()->get_id() != LET) {
            error_at(lexer.peek_token()->get_locus(),
              "if expression probably exists, but is being parsed as an if let expression. This may "
              "be a parser error.");
            // skip somewhere?
            return NULL;
        }
        lexer.skip_token();

        // parse match arm patterns (which are required)
        ::std::vector< ::std::unique_ptr<AST::Pattern> > match_arm_patterns
          = parse_match_arm_patterns(EQUAL);
        if (match_arm_patterns.empty()) {
            error_at(lexer.peek_token()->get_locus(),
              "failed to parse any match arm patterns in if let expression");
            // skip somewhere?
            return NULL;
        }

        if (!skip_token(EQUAL)) {
            // skip somewhere?
            return NULL;
        }

        // parse expression (required) - HACK to prevent struct expr being parsed
        ParseRestrictions no_struct_expr;
        no_struct_expr.can_be_struct_expr = false;
        ::std::unique_ptr<AST::Expr> scrutinee_expr
          = parse_expr(::std::vector<AST::Attribute>(), no_struct_expr);
        if (scrutinee_expr == NULL) {
            error_at(lexer.peek_token()->get_locus(),
              "failed to parse scrutinee expression in if let expression");
            // skip somewhere?
            return NULL;
        }
        /* TODO: check for expression not being a struct expression or lazy boolean expression here?
         * or actually probably in semantic analysis. */

        // parse block expression (required)
        ::std::unique_ptr<AST::BlockExpr> if_let_body = parse_block_expr();
        if (if_let_body == NULL) {
            error_at(lexer.peek_token()->get_locus(),
              "failed to parse if let body block expression in if let expression");
            // skip somewhere?
            return NULL;
        }

        // branch to parse end or else (and then else, else if, or else if let)
        if (lexer.peek_token()->get_id() != ELSE) {
            // single selection - end of if let expression
            return ::std::unique_ptr<AST::IfLetExpr>(
              new AST::IfLetExpr(::std::move(match_arm_patterns), ::std::move(scrutinee_expr),
                ::std::move(if_let_body), locus));
        } else {
            // double or multiple selection - branch on end, else if, or else if let

            // skip "else"
            lexer.skip_token();

            // branch on whether next token is '{' or 'if'
            const_TokenPtr t = lexer.peek_token();
            switch (t->get_id()) {
                case LEFT_CURLY: {
                    // double selection - else
                    // parse else block expr (required)
                    ::std::unique_ptr<AST::BlockExpr> else_body = parse_block_expr();
                    if (else_body == NULL) {
                        error_at(lexer.peek_token()->get_locus(),
                          "failed to parse else body block expression in if let expression");
                        // skip somewhere?
                        return NULL;
                    }

                    return ::std::unique_ptr<AST::IfLetExprConseqElse>(new AST::IfLetExprConseqElse(
                      ::std::move(match_arm_patterns), ::std::move(scrutinee_expr),
                      ::std::move(if_let_body), ::std::move(else_body), locus));
                }
                case IF: {
                    // multiple selection - else if or else if let
                    // branch on whether next token is 'let' or not
                    if (lexer.peek_token(1)->get_id() == LET) {
                        // parse if let expr (required)
                        ::std::unique_ptr<AST::IfLetExpr> if_let_expr = parse_if_let_expr();
                        if (if_let_expr == NULL) {
                            error_at(lexer.peek_token()->get_locus(),
                              "failed to parse (else) if let expression after if let expression");
                            // skip somewhere?
                            return NULL;
                        }

                        return ::std::unique_ptr<AST::IfLetExprConseqIfLet>(
                          new AST::IfLetExprConseqIfLet(::std::move(match_arm_patterns),
                            ::std::move(scrutinee_expr), ::std::move(if_let_body),
                            ::std::move(if_let_expr), locus));
                    } else {
                        // parse if expr (required)
                        ::std::unique_ptr<AST::IfExpr> if_expr = parse_if_expr();
                        if (if_expr == NULL) {
                            error_at(lexer.peek_token()->get_locus(),
                              "failed to parse (else) if expression after if let expression");
                            // skip somewhere?
                            return NULL;
                        }

                        return ::std::unique_ptr<AST::IfLetExprConseqIf>(new AST::IfLetExprConseqIf(
                          ::std::move(match_arm_patterns), ::std::move(scrutinee_expr),
                          ::std::move(if_let_body), ::std::move(if_expr), locus));
                    }
                }
                default:
                    // error - invalid token
                    error_at(t->get_locus(), "unexpected token '%s' after else in if let expression",
                      t->get_token_description());
                    // skip somewhere?
                    return NULL;
            }
        }
    }

    // TODO: possibly decide on different method of handling label (i.e. not parameter)

    /* Parses a "loop" infinite loop expression. Label is not parsed and should be parsed via
     * parse_labelled_loop_expr, which would call this. */
    ::std::unique_ptr<AST::LoopExpr> Parser::parse_loop_expr(
      ::std::vector<AST::Attribute> outer_attrs, AST::LoopLabel label) {
        location_t locus = UNKNOWN_LOCATION;
        if (label.is_error()) {
            locus = lexer.peek_token()->get_locus();
        } else {
            locus = label.get_locus();
        }
        skip_token(LOOP);

        // parse loop body, which is required
        ::std::unique_ptr<AST::BlockExpr> loop_body = parse_block_expr();
        if (loop_body == NULL) {
            error_at(lexer.peek_token()->get_locus(),
              "could not parse loop body in (infinite) loop expression");
            return NULL;
        }

        return ::std::unique_ptr<AST::LoopExpr>(new AST::LoopExpr(
          ::std::move(loop_body), locus, ::std::move(label), ::std::move(outer_attrs)));
    }

    /* Parses a "while" loop expression. Label is not parsed and should be parsed via
     * parse_labelled_loop_expr, which would call this. */
    ::std::unique_ptr<AST::WhileLoopExpr> Parser::parse_while_loop_expr(
      ::std::vector<AST::Attribute> outer_attrs, AST::LoopLabel label) {
        location_t locus = UNKNOWN_LOCATION;
        if (label.is_error()) {
            locus = lexer.peek_token()->get_locus();
        } else {
            locus = label.get_locus();
        }
        skip_token(WHILE);

        // ensure it isn't a while let loop
        if (lexer.peek_token()->get_id() == LET) {
            error_at(lexer.peek_token()->get_locus(),
              "appears to be while let loop but is being parsed by "
              "while loop - this may be a compiler issue");
            // skip somewhere?
            return NULL;
        }

        // parse loop predicate (required) with HACK to prevent struct expr parsing
        ParseRestrictions no_struct_expr;
        no_struct_expr.can_be_struct_expr = false;
        ::std::unique_ptr<AST::Expr> predicate
          = parse_expr(::std::vector<AST::Attribute>(), no_struct_expr);
        if (predicate == NULL) {
            error_at(
              lexer.peek_token()->get_locus(), "failed to parse predicate expression in while loop");
            // skip somewhere?
            return NULL;
        }
        // TODO: check that it isn't struct expression here? actually, probably in semantic analysis

        // parse loop body (required)
        ::std::unique_ptr<AST::BlockExpr> body = parse_block_expr();
        if (body == NULL) {
            error_at(lexer.peek_token()->get_locus(),
              "failed to parse loop body block expression in while loop");
            // skip somewhere
            return NULL;
        }

        return ::std::unique_ptr<AST::WhileLoopExpr>(new AST::WhileLoopExpr(::std::move(predicate),
          ::std::move(body), locus, ::std::move(label), ::std::move(outer_attrs)));
    }

    /* Parses a "while let" loop expression. Label is not parsed and should be parsed via
     * parse_labelled_loop_expr, which would call this. */
    ::std::unique_ptr<AST::WhileLetLoopExpr> Parser::parse_while_let_loop_expr(
      ::std::vector<AST::Attribute> outer_attrs, AST::LoopLabel label) {
        location_t locus = UNKNOWN_LOCATION;
        if (label.is_error()) {
            locus = lexer.peek_token()->get_locus();
        } else {
            locus = label.get_locus();
        }
        skip_token(WHILE);

        // check for possible accidental recognition of a while loop as a while let loop
        if (lexer.peek_token()->get_id() != LET) {
            error_at(lexer.peek_token()->get_locus(),
              "appears to be a while loop but is being parsed by "
              "while let loop - this may be a compiler issue");
            // skip somewhere
            return NULL;
        }
        // as this token is definitely let now, save the computation of comparison
        lexer.skip_token();

        // parse predicate patterns
        ::std::vector< ::std::unique_ptr<AST::Pattern> > predicate_patterns
          = parse_match_arm_patterns(EQUAL);
        // TODO: have to ensure that there is at least 1 pattern?

        if (!skip_token(EQUAL)) {
            // skip somewhere?
            return NULL;
        }

        // parse predicate expression, which is required (and HACK to prevent struct expr)
        ParseRestrictions no_struct_expr;
        no_struct_expr.can_be_struct_expr = false;
        ::std::unique_ptr<AST::Expr> predicate_expr
          = parse_expr(::std::vector<AST::Attribute>(), no_struct_expr);
        if (predicate_expr == NULL) {
            error_at(lexer.peek_token()->get_locus(),
              "failed to parse predicate expression in while let loop");
            // skip somewhere?
            return NULL;
        }
        // TODO: ensure that struct expression is not parsed? Actually, probably in semantic analysis.

        // parse loop body, which is required
        ::std::unique_ptr<AST::BlockExpr> body = parse_block_expr();
        if (body == NULL) {
            error_at(lexer.peek_token()->get_locus(),
              "failed to parse block expr (loop body) of while let loop");
            // skip somewhere?
            return NULL;
        }

        return ::std::unique_ptr<AST::WhileLetLoopExpr>(
          new AST::WhileLetLoopExpr(::std::move(predicate_patterns), ::std::move(predicate_expr),
            ::std::move(body), locus, ::std::move(label), ::std::move(outer_attrs)));
    }

    /* Parses a "for" iterative loop. Label is not parsed and should be parsed via
     * parse_labelled_loop_expr, which would call this. */
    ::std::unique_ptr<AST::ForLoopExpr> Parser::parse_for_loop_expr(
      ::std::vector<AST::Attribute> outer_attrs, AST::LoopLabel label) {
        location_t locus = UNKNOWN_LOCATION;
        if (label.is_error()) {
            locus = lexer.peek_token()->get_locus();
        } else {
            locus = label.get_locus();
        }
        skip_token(FOR);

        // parse pattern, which is required
        ::std::unique_ptr<AST::Pattern> pattern = parse_pattern();
        if (pattern == NULL) {
            error_at(lexer.peek_token()->get_locus(), "failed to parse iterator pattern in for loop");
            // skip somewhere?
            return NULL;
        }

        if (!skip_token(IN)) {
            // skip somewhere?
            return NULL;
        }

        // parse iterator expression, which is required - also HACK to prevent struct expr
        ParseRestrictions no_struct_expr;
        no_struct_expr.can_be_struct_expr = false;
        ::std::unique_ptr<AST::Expr> expr
          = parse_expr(::std::vector<AST::Attribute>(), no_struct_expr);
        if (expr == NULL) {
            error_at(
              lexer.peek_token()->get_locus(), "failed to parse iterator expression in for loop");
            // skip somewhere?
            return NULL;
        }
        // TODO: check to ensure this isn't struct expr? Or in semantic analysis.

        // parse loop body, which is required
        ::std::unique_ptr<AST::BlockExpr> body = parse_block_expr();
        if (body == NULL) {
            error_at(lexer.peek_token()->get_locus(),
              "failed to parse loop body block expression in for loop");
            // skip somewhere?
            return NULL;
        }

        return ::std::unique_ptr<AST::ForLoopExpr>(new AST::ForLoopExpr(::std::move(pattern),
          ::std::move(expr), ::std::move(body), locus, ::std::move(label), ::std::move(outer_attrs)));
    }

    // Parses a loop expression with label (any kind of loop - disambiguates).
    ::std::unique_ptr<AST::BaseLoopExpr> Parser::parse_labelled_loop_expr(
      ::std::vector<AST::Attribute> outer_attrs) {
        // TODO: decide whether it should not work if there is no label, or parse it with no label
        // at the moment, I will make it not work with no label because that's the implication.

        if (lexer.peek_token()->get_id() != LIFETIME) {
            error_at(lexer.peek_token()->get_locus(),
              "expected lifetime in labelled loop expr (to parse loop label) - found '%s'",
              lexer.peek_token()->get_token_description());
            // skip?
            return NULL;
        }

        // parse loop label (required)
        AST::LoopLabel label = parse_loop_label();
        if (label.is_error()) {
            error_at(
              lexer.peek_token()->get_locus(), "failed to parse loop label in labelled loop expr");
            // skip?
            return NULL;
        }

        // branch on next token
        const_TokenPtr t = lexer.peek_token();
        switch (t->get_id()) {
            case LOOP:
                return parse_loop_expr(::std::move(outer_attrs), ::std::move(label));
            case FOR:
                return parse_for_loop_expr(::std::move(outer_attrs), ::std::move(label));
            case WHILE:
                // further disambiguate into while vs while let
                if (lexer.peek_token(1)->get_id() == LET) {
                    return parse_while_let_loop_expr(::std::move(outer_attrs), ::std::move(label));
                } else {
                    return parse_while_loop_expr(::std::move(outer_attrs), ::std::move(label));
                }
            default:
                // error
                error_at(t->get_locus(), "unexpected token '%s' when parsing labelled loop",
                  t->get_token_description());
                // skip?
                return NULL;
        }
    }

    // Parses a match expression.
    ::std::unique_ptr<AST::MatchExpr> Parser::parse_match_expr(
      ::std::vector<AST::Attribute> outer_attrs, bool pratt_parse) {
        location_t locus = UNKNOWN_LOCATION;
        if (!pratt_parse) {
            locus = lexer.peek_token()->get_locus();

            skip_token(MATCH_TOK);
        } else {
            // TODO: probably just pass in location data as param
            // get current pos then move back 6 - 5 for match, 1 for space
            locus = lexer.peek_token()->get_locus() - 6;
        }

        // parse scrutinee expression, which is required (and HACK to prevent struct expr)
        ParseRestrictions no_struct_expr;
        no_struct_expr.can_be_struct_expr = false;
        ::std::unique_ptr<AST::Expr> scrutinee
          = parse_expr(::std::vector<AST::Attribute>(), no_struct_expr);
        if (scrutinee == NULL) {
            error_at(lexer.peek_token()->get_locus(),
              "failed to parse scrutinee expression in match expression");
            // skip somewhere?
            return NULL;
        }
        // TODO: check for scrutinee expr not being struct expr? or do so in semantic analysis

        if (!skip_token(LEFT_CURLY)) {
            // skip somewhere?
            return NULL;
        }

        // parse inner attributes (if they exist)
        ::std::vector<AST::Attribute> inner_attrs = parse_inner_attributes();

        // parse match arms (if they exist)
        ::std::vector< ::std::unique_ptr<AST::MatchCase> > match_arms;

        // DEBUG
        fprintf(stderr, "about to start loop to start parsing match cases\n");

        // FIXME: absolute worst control structure ever
        // parse match cases
        while (true) {
            // parse match arm itself, which is required
            AST::MatchArm arm = parse_match_arm();
            if (arm.is_error()) {
                // DEBUG
                fprintf(stderr, "broke loop on invalid match arm\n");

                // not necessarily an error
                break;
            }

            // DEBUG
            fprintf(stderr, "parsed valid match arm\n");

            if (!skip_token(MATCH_ARROW)) {
                // skip after somewhere?
                // TODO is returning here a good idea? or is break better?
                return NULL;
            }

            // DEBUG
            fprintf(stderr, "skipped match arrow\n");

            // branch on next token - if '{', block expr, otherwise just expr
            if (lexer.peek_token()->get_id() == LEFT_CURLY) {
                // block expr
                ::std::unique_ptr<AST::BlockExpr> block_expr = parse_block_expr();
                if (block_expr == NULL) {
                    error_at(lexer.peek_token()->get_locus(),
                      "failed to parse block expr in match arm in match expr");
                    // skip somewhere
                    return NULL;
                }

                // create match case block expr and add to cases
                ::std::unique_ptr<AST::MatchCaseBlockExpr> match_case_block(
                  new AST::MatchCaseBlockExpr(::std::move(arm), ::std::move(block_expr)));
                match_arms.push_back(::std::move(match_case_block));

                // skip optional comma
                if (lexer.peek_token()->get_id() == COMMA) {
                    lexer.skip_token();
                }
            } else {
                // regular expr
                ::std::unique_ptr<AST::Expr> expr = parse_expr();
                if (expr == NULL) {
                    error_at(lexer.peek_token()->get_locus(),
                      "failed to parse expr in match arm in match expr");
                    // skip somewhere?
                    return NULL;
                }

                // construct match case expr and add to cases
                ::std::unique_ptr<AST::MatchCaseExpr> match_case_expr(
                  new AST::MatchCaseExpr(::std::move(arm), ::std::move(expr)));
                match_arms.push_back(::std::move(match_case_expr));

                // skip REQUIRED comma - if no comma, break
                if (lexer.peek_token()->get_id() != COMMA) {
                    // if no comma, must be end of cases
                    break;
                }
                lexer.skip_token();
            }
        }

        if (!skip_token(RIGHT_CURLY)) {
            // skip somewhere?
            return NULL;
        }

        return ::std::unique_ptr<AST::MatchExpr>(new AST::MatchExpr(::std::move(scrutinee),
          ::std::move(match_arms), ::std::move(inner_attrs), ::std::move(outer_attrs), locus));
    }

    // Parses the "pattern" part of the match arm (the 'case x:' equivalent).
    AST::MatchArm Parser::parse_match_arm() {
        // parse optional outer attributes
        ::std::vector<AST::Attribute> outer_attrs = parse_outer_attributes();

        // DEBUG
        fprintf(stderr, "about to start parsing match arm patterns\n");

        // break early if find right curly
        if (lexer.peek_token()->get_id() == RIGHT_CURLY) {
            // not an error
            return AST::MatchArm::create_error();
        }

        // parse match arm patterns - at least 1 is required
        ::std::vector< ::std::unique_ptr<AST::Pattern> > match_arm_patterns
          = parse_match_arm_patterns(RIGHT_CURLY);
        if (match_arm_patterns.empty()) {
            error_at(lexer.peek_token()->get_locus(), "failed to parse any patterns in match arm");
            // skip somewhere?
            return AST::MatchArm::create_error();
        }

        // DEBUG
        fprintf(stderr, "successfully parsed match arm patterns\n");

        // parse match arm guard expr if it exists
        ::std::unique_ptr<AST::Expr> guard_expr = NULL;
        if (lexer.peek_token()->get_id() == IF) {
            lexer.skip_token();

            guard_expr = parse_expr();
            if (guard_expr == NULL) {
                error_at(
                  lexer.peek_token()->get_locus(), "failed to parse guard expression in match arm");
                // skip somewhere?
                return AST::MatchArm::create_error();
            }
        }

        // DEBUG
        fprintf(stderr, "successfully parsed match arm\n");

        return AST::MatchArm(
          ::std::move(match_arm_patterns), ::std::move(guard_expr), ::std::move(outer_attrs));
    }

    /* Parses the patterns used in a match arm. End token id is the id of the token that would exist
     * after the patterns are done (e.g. '}' for match expr, '=' for if let and while let). */
    ::std::vector< ::std::unique_ptr<AST::Pattern> > Parser::parse_match_arm_patterns(
      TokenId end_token_id) {
        // skip optional leading '|'
        bool has_leading_pipe = false;
        if (lexer.peek_token()->get_id() == PIPE) {
            has_leading_pipe = true;
            lexer.skip_token();
        }
        // TODO: do I even need to store the result of this? can't be used.
        // If semantically different, I need a wrapped "match arm patterns" object for this.

        ::std::vector< ::std::unique_ptr<AST::Pattern> > patterns;

        // quick break out if end_token_id
        if (lexer.peek_token()->get_id() == end_token_id) {
            return patterns;
        }

        // parse required pattern - if doesn't exist, return empty
        ::std::unique_ptr<AST::Pattern> initial_pattern = parse_pattern();
        if (initial_pattern == NULL) {
            // FIXME: should this be an error?
            return patterns;
        }
        patterns.push_back(::std::move(initial_pattern));

        // DEBUG
        fprintf(stderr, "successfully parsed initial match arm pattern\n");

        // parse new patterns as long as next char is '|'
        const_TokenPtr t = lexer.peek_token();
        while (t->get_id() == PIPE) {
            // skip pipe token
            lexer.skip_token();

            // break if hit end token id
            if (lexer.peek_token()->get_id() == end_token_id) {
                break;
            }

            // parse pattern
            ::std::unique_ptr<AST::Pattern> pattern = parse_pattern();
            if (pattern == NULL) {
                // this is an error
                error_at(
                  lexer.peek_token()->get_locus(), "failed to parse pattern in match arm patterns");
                // skip somewhere?
                return ::std::vector< ::std::unique_ptr<AST::Pattern> >();
            }

            patterns.push_back(::std::move(pattern));

            t = lexer.peek_token();
        }

        return patterns;
    }

    // Parses an async block expression.
    ::std::unique_ptr<AST::AsyncBlockExpr> Parser::parse_async_block_expr(
      ::std::vector<AST::Attribute> outer_attrs) {
        location_t locus = lexer.peek_token()->get_locus();
        skip_token(ASYNC);

        // detect optional move token
        bool has_move = false;
        if (lexer.peek_token()->get_id() == MOVE) {
            lexer.skip_token();
            has_move = true;
        }

        // parse block expression (required)
        ::std::unique_ptr<AST::BlockExpr> block_expr = parse_block_expr();
        if (block_expr == NULL) {
            error_at(lexer.peek_token()->get_locus(),
              "failed to parse block expression of async block expression");
            // skip somewhere?
            return NULL;
        }

        return ::std::unique_ptr<AST::AsyncBlockExpr>(new AST::AsyncBlockExpr(
          ::std::move(block_expr), has_move, ::std::move(outer_attrs), locus));
    }

    // Parses an unsafe block expression.
    ::std::unique_ptr<AST::UnsafeBlockExpr> Parser::parse_unsafe_block_expr(
      ::std::vector<AST::Attribute> outer_attrs) {
        location_t locus = lexer.peek_token()->get_locus();
        skip_token(UNSAFE);

        // parse block expression (required)
        ::std::unique_ptr<AST::BlockExpr> block_expr = parse_block_expr();
        if (block_expr == NULL) {
            error_at(lexer.peek_token()->get_locus(),
              "failed to parse block expression of unsafe block expression");
            // skip somewhere?
            return NULL;
        }

        return ::std::unique_ptr<AST::UnsafeBlockExpr>(
          new AST::UnsafeBlockExpr(::std::move(block_expr), ::std::move(outer_attrs), locus));
    }

    // Parses an array definition expression.
    ::std::unique_ptr<AST::ArrayExpr> Parser::parse_array_expr(
      ::std::vector<AST::Attribute> outer_attrs, bool pratt_parse) {
        location_t locus = UNKNOWN_LOCATION;
        if (!pratt_parse) {
            locus = lexer.peek_token()->get_locus();

            skip_token(LEFT_SQUARE);
        } else {
            locus = lexer.peek_token()->get_locus() - 1;
        }

        // parse optional inner attributes
        ::std::vector<AST::Attribute> inner_attrs = parse_inner_attributes();

        // parse the "array elements" section, which is optional
        if (lexer.peek_token()->get_id() == RIGHT_SQUARE) {
            // no array elements
            lexer.skip_token();

            return ::std::unique_ptr<AST::ArrayExpr>(
              new AST::ArrayExpr(NULL, ::std::move(inner_attrs), ::std::move(outer_attrs), locus));
        } else {
            // should have array elements
            // parse initial expression, which is required for either
            ::std::unique_ptr<AST::Expr> initial_expr = parse_expr();
            if (initial_expr == NULL) {
                error_at(lexer.peek_token()->get_locus(),
                  "could not parse expression in array expression "
                  "(even though arrayelems seems to be present)");
                // skip somewhere?
                return NULL;
            }

            if (lexer.peek_token()->get_id() == SEMICOLON) {
                // copy array elems
                lexer.skip_token();

                // parse copy amount expression (required)
                ::std::unique_ptr<AST::Expr> copy_amount = parse_expr();
                if (copy_amount == NULL) {
                    error_at(lexer.peek_token()->get_locus(),
                      "could not parse copy amount expression in array expression (arrayelems)");
                    // skip somewhere?
                    return NULL;
                }

                ::std::unique_ptr<AST::ArrayElemsCopied> copied_array_elems(
                  new AST::ArrayElemsCopied(::std::move(initial_expr), ::std::move(copy_amount)));
                return ::std::unique_ptr<AST::ArrayExpr>(
                  new AST::ArrayExpr(::std::move(copied_array_elems), ::std::move(inner_attrs),
                    ::std::move(outer_attrs), locus));
            } else if (lexer.peek_token()->get_id() == RIGHT_SQUARE) {
                // single-element array expression
                ::std::vector< ::std::unique_ptr<AST::Expr> > exprs;
                exprs.push_back(::std::move(initial_expr));

                ::std::unique_ptr<AST::ArrayElemsValues> array_elems(
                  new AST::ArrayElemsValues(::std::move(exprs)));
                return ::std::unique_ptr<AST::ArrayExpr>(new AST::ArrayExpr(::std::move(array_elems),
                  ::std::move(inner_attrs), ::std::move(outer_attrs), locus));
            } else if (lexer.peek_token()->get_id() == COMMA) {
                // multi-element array expression (or trailing comma)
                ::std::vector< ::std::unique_ptr<AST::Expr> > exprs;
                exprs.push_back(::std::move(initial_expr));

                const_TokenPtr t = lexer.peek_token();
                while (t->get_id() == COMMA) {
                    lexer.skip_token();

                    // quick break if right square bracket
                    if (lexer.peek_token()->get_id() == RIGHT_SQUARE) {
                        break;
                    }

                    // parse expression (required)
                    ::std::unique_ptr<AST::Expr> expr = parse_expr();
                    if (expr == NULL) {
                        error_at(lexer.peek_token()->get_locus(),
                          "failed to parse element in array expression");
                        // skip somewhere?
                        return NULL;
                    }
                    exprs.push_back(::std::move(expr));

                    t = lexer.peek_token();
                }

                skip_token(RIGHT_SQUARE);

                ::std::unique_ptr<AST::ArrayElemsValues> array_elems(
                  new AST::ArrayElemsValues(::std::move(exprs)));
                return ::std::unique_ptr<AST::ArrayExpr>(new AST::ArrayExpr(::std::move(array_elems),
                  ::std::move(inner_attrs), ::std::move(outer_attrs), locus));
            } else {
                // error
                error_at(lexer.peek_token()->get_locus(),
                  "unexpected token '%s' in array expression (arrayelems)",
                  lexer.peek_token()->get_token_description());
                // skip somewhere?
                return NULL;
            }
        }
    }

    // Parses a single parameter used in a closure definition.
    AST::ClosureParam Parser::parse_closure_param() {
        // parse pattern (which is required)
        ::std::unique_ptr<AST::Pattern> pattern = parse_pattern();
        if (pattern == NULL) {
            // not necessarily an error
            return AST::ClosureParam::create_error();
        }

        // parse optional type of param
        ::std::unique_ptr<AST::Type> type = NULL;
        if (lexer.peek_token()->get_id() == COLON) {
            lexer.skip_token();

            // parse type, which is now required
            type = parse_type();
            if (type == NULL) {
                error_at(lexer.peek_token()->get_id(), "failed to parse type in closure parameter");
                // skip somewhere?
                return AST::ClosureParam::create_error();
            }
        }

        return AST::ClosureParam(::std::move(pattern), ::std::move(type));
    }

    // Parses a grouped or tuple expression (disambiguates).
    ::std::unique_ptr<AST::ExprWithoutBlock> Parser::parse_grouped_or_tuple_expr(
      ::std::vector<AST::Attribute> outer_attrs, bool pratt_parse) {
        // adjustment to allow Pratt parsing to reuse function without copy-paste
        location_t locus = UNKNOWN_LOCATION;
        if (!pratt_parse) {
            locus = lexer.peek_token()->get_locus();

            skip_token(LEFT_PAREN);
        } else {
            locus = lexer.peek_token()->get_locus() - 1;
        }

        // parse optional inner attributes
        ::std::vector<AST::Attribute> inner_attrs = parse_inner_attributes();

        if (lexer.peek_token()->get_id() == RIGHT_PAREN) {
            // must be empty tuple
            lexer.skip_token();

            // create tuple with empty tuple elems
            return ::std::unique_ptr<AST::TupleExpr>(
              new AST::TupleExpr(::std::vector< ::std::unique_ptr<AST::Expr> >(),
                ::std::move(inner_attrs), ::std::move(outer_attrs), locus));
        }

        // parse first expression (required)
        ::std::unique_ptr<AST::Expr> first_expr = parse_expr();
        if (first_expr == NULL) {
            error_at(lexer.peek_token()->get_locus(),
              "failed to parse expression in grouped or tuple expression");
            // skip after somewhere?
            return NULL;
        }

        // detect whether grouped expression with right parentheses as next token
        if (lexer.peek_token()->get_id() == RIGHT_PAREN) {
            // must be grouped expr
            lexer.skip_token();

            // create grouped expr
            return ::std::unique_ptr<AST::GroupedExpr>(new AST::GroupedExpr(
              ::std::move(first_expr), ::std::move(inner_attrs), ::std::move(outer_attrs), locus));
        } else if (lexer.peek_token()->get_id() == COMMA) {
            // tuple expr
            ::std::vector< ::std::unique_ptr<AST::Expr> > exprs;
            exprs.push_back(::std::move(first_expr));

            // parse potential other tuple exprs
            const_TokenPtr t = lexer.peek_token();
            while (t->get_id() == COMMA) {
                lexer.skip_token();

                // break out if right paren
                if (lexer.peek_token()->get_id() == RIGHT_PAREN) {
                    break;
                }

                // parse expr, which is now required
                ::std::unique_ptr<AST::Expr> expr = parse_expr();
                if (expr == NULL) {
                    error_at(lexer.peek_token()->get_locus(), "failed to parse expr in tuple expr");
                    // skip somewhere?
                    return NULL;
                }
                exprs.push_back(::std::move(expr));

                t = lexer.peek_token();
            }

            // skip right paren
            skip_token(RIGHT_PAREN);

            return ::std::unique_ptr<AST::TupleExpr>(new AST::TupleExpr(
              ::std::move(exprs), ::std::move(inner_attrs), ::std::move(outer_attrs), locus));
        } else {
            // error
            const_TokenPtr t = lexer.peek_token();
            error_at(t->get_locus(),
              "unexpected token '%s' in grouped or tuple expression (parenthesised expression) - "
              "expected ')' for grouped expr and ',' for tuple expr",
              t->get_token_description());
            // skip somewhere?
            return NULL;
        }
    }

    // Parses a type (will further disambiguate any type).
    ::std::unique_ptr<AST::Type> Parser::parse_type() {
        /* rules for all types:
         * NeverType:               '!'
         * SliceType:               '[' Type ']'
         * InferredType:            '_'
         * MacroInvocation:         SimplePath '!' DelimTokenTree
         * ParenthesisedType:       '(' Type ')'
         * ImplTraitType:           'impl' TypeParamBounds
         *  TypeParamBounds (not type)  TypeParamBound ( '+' TypeParamBound )* '+'?
         *  TypeParamBound          Lifetime | TraitBound
         * ImplTraitTypeOneBound:   'impl' TraitBound
         * TraitObjectType:         'dyn'? TypeParamBounds
         * TraitObjectTypeOneBound: 'dyn'? TraitBound
         *  TraitBound              '?'? ForLifetimes? TypePath | '(' '?'? ForLifetimes? TypePath ')'
         * BareFunctionType:        ForLifetimes? FunctionQualifiers 'fn' etc.
         *  ForLifetimes (not type) 'for' '<' LifetimeParams '>'
         *  FunctionQualifiers      ( 'async' | 'const' )? 'unsafe'? ('extern' abi?)?
         * QualifiedPathInType:     '<' Type ( 'as' TypePath )? '>' ( '::' TypePathSegment )+
         * TypePath:                '::'? TypePathSegment ( '::' TypePathSegment)*
         * ArrayType:               '[' Type ';' Expr ']'
         * ReferenceType:           '&' Lifetime? 'mut'? TypeNoBounds
         * RawPointerType:          '*' ( 'mut' | 'const' ) TypeNoBounds
         * TupleType:               '(' Type etc. - regular tuple stuff. Also regular tuple vs
         *                              parenthesised precedence
         *
         * Disambiguate between macro and type path via type path being parsed, and then if '!'
         * found, convert type path to simple path for macro.
         * Usual disambiguation for tuple vs parenthesised.
         * For ImplTraitType and TraitObjectType individual disambiguations, they seem more like
         * "special cases", so probably just try to parse the more general ImplTraitType or
         * TraitObjectType and return OneBound versions if they satisfy those criteria. */

        const_TokenPtr t = lexer.peek_token();
        switch (t->get_id()) {
            case EXCLAM:
                // never type - can't be macro as no path beforehand
                lexer.skip_token();
                return ::std::unique_ptr<AST::NeverType>(new AST::NeverType(t->get_locus()));
            case LEFT_SQUARE:
                // slice type or array type - requires further disambiguation
                return parse_slice_or_array_type();
            case LEFT_ANGLE: {
                // qualified path in type
                AST::QualifiedPathInType path = parse_qualified_path_in_type();
                if (path.is_error()) {
                    error_at(t->get_locus(), "failed to parse qualified path in type");
                    return NULL;
                }
                return ::std::unique_ptr<AST::QualifiedPathInType>(
                  new AST::QualifiedPathInType(::std::move(path)));
            }
            case UNDERSCORE:
                // inferred type
                lexer.skip_token();
                return ::std::unique_ptr<AST::InferredType>(new AST::InferredType(t->get_locus()));
            case ASTERISK:
                // raw pointer type
                return parse_raw_pointer_type();
            case AMP: // does this also include AMP_AMP?
                // reference type
                return parse_reference_type();
            case LIFETIME: {
                // probably a lifetime bound, so probably type param bounds in TraitObjectType
                ::std::vector< ::std::unique_ptr<AST::TypeParamBound> > bounds
                  = parse_type_param_bounds();

                return ::std::unique_ptr<AST::TraitObjectType>(
                  new AST::TraitObjectType(::std::move(bounds), t->get_locus()));
            }
            case IDENTIFIER:
            case SUPER:
            case SELF:
            case SELF_ALIAS:
            case CRATE:
            case DOLLAR_SIGN:
            case SCOPE_RESOLUTION: {
                // macro invocation or type path - requires further disambiguation.
                /* for parsing path component of each rule, perhaps parse it as a typepath and
                 * attempt conversion to simplepath if a trailing '!' is found */
                /* Type path also includes TraitObjectTypeOneBound BUT if it starts with it, it is
                 * exactly the same as a TypePath syntactically, so this is a syntactical ambiguity.
                 * As such, the parser will parse it as a TypePath.
                 * This, however, does not prevent TraitObjectType from starting with a typepath. */

                // parse path as type path
                AST::TypePath path = parse_type_path();
                if (path.is_error()) {
                    error_at(t->get_locus(), "failed to parse path as first component of type");
                    return NULL;
                }
                location_t locus = path.get_locus();

                // branch on next token
                t = lexer.peek_token();
                switch (t->get_id()) {
                    case EXCLAM: {
                        // macro invocation
                        // convert to simple path
                        AST::SimplePath macro_path = path.as_simple_path();
                        if (macro_path.is_empty()) {
                            error_at(t->get_locus(),
                              "failed to parse simple path in macro invocation (for type)");
                            return NULL;
                        }

                        lexer.skip_token();

                        AST::DelimTokenTree tok_tree = parse_delim_token_tree();

                        return ::std::unique_ptr<AST::MacroInvocation>(
                          new AST::MacroInvocation(::std::move(macro_path), ::std::move(tok_tree),
                            ::std::vector<AST::Attribute>(), locus));
                    }
                    case PLUS: {
                        // type param bounds
                        ::std::vector< ::std::unique_ptr<AST::TypeParamBound> > bounds;

                        // convert type path to trait bound
                        ::std::unique_ptr<AST::TraitBound> path_bound(
                          new AST::TraitBound(::std::move(path), locus, false, false));
                        bounds.push_back(::std::move(path_bound));

                        // parse rest of bounds - FIXME: better way to find when to stop parsing
                        while (t->get_id() == PLUS) {
                            lexer.skip_token();

                            // parse bound if it exists - if not, assume end of sequence
                            ::std::unique_ptr<AST::TypeParamBound> bound = parse_type_param_bound();
                            if (bound == NULL) {
                                break;
                            }
                            bounds.push_back(::std::move(bound));

                            t = lexer.peek_token();
                        }

                        return ::std::unique_ptr<AST::TraitObjectType>(
                          new AST::TraitObjectType(::std::move(bounds), locus));
                    }
                    default:
                        // assume that this is a type path and not an error
                        return ::std::unique_ptr<AST::TypePath>(new AST::TypePath(::std::move(path)));
                }
            }
            case LEFT_PAREN:
                // tuple type or parenthesised type - requires further disambiguation (the usual)
                // ok apparently can be a parenthesised TraitBound too, so could be
                // TraitObjectTypeOneBound or TraitObjectType
                return parse_paren_prefixed_type();
            case FOR:
                // TraitObjectTypeOneBound or BareFunctionType
                return parse_for_prefixed_type();
            case ASYNC:
            case CONST:
            case UNSAFE:
            case EXTERN_TOK:
            case FN_TOK:
                // bare function type (with no for lifetimes)
                return parse_bare_function_type(::std::vector<AST::LifetimeParam>());
            case IMPL:
                lexer.skip_token();
                if (lexer.peek_token()->get_id() == LIFETIME) {
                    // cannot be one bound because lifetime prevents it from being traitbound
                    ::std::vector< ::std::unique_ptr<AST::TypeParamBound> > bounds
                      = parse_type_param_bounds();

                    return ::std::unique_ptr<AST::ImplTraitType>(
                      new AST::ImplTraitType(::std::move(bounds), t->get_locus()));
                } else {
                    // should be trait bound, so parse trait bound
                    ::std::unique_ptr<AST::TraitBound> initial_bound = parse_trait_bound();
                    if (initial_bound == NULL) {
                        error_at(lexer.peek_token()->get_locus(),
                          "failed to parse ImplTraitType initial bound");
                        return NULL;
                    }

                    location_t locus = t->get_locus();

                    // short cut if next token isn't '+'
                    t = lexer.peek_token();
                    if (t->get_id() != PLUS) {
                        // convert trait bound to value object
                        AST::TraitBound value_bound(*initial_bound);

                        // DEBUG: removed as unique ptr, so should auto-delete
                        // delete initial_bound;

                        return ::std::unique_ptr<AST::ImplTraitTypeOneBound>(
                          new AST::ImplTraitTypeOneBound(::std::move(value_bound), locus));
                    }

                    // parse additional type param bounds
                    ::std::vector< ::std::unique_ptr<AST::TypeParamBound> > bounds;
                    bounds.push_back(::std::move(initial_bound));
                    while (t->get_id() == PLUS) {
                        lexer.skip_token();

                        // parse bound if it exists
                        ::std::unique_ptr<AST::TypeParamBound> bound = parse_type_param_bound();
                        if (bound == NULL) {
                            // not an error as trailing plus may exist
                            break;
                        }
                        bounds.push_back(::std::move(bound));

                        t = lexer.peek_token();
                    }

                    return ::std::unique_ptr<AST::ImplTraitType>(
                      new AST::ImplTraitType(::std::move(bounds), locus));
                }
            case DYN:
            case QUESTION_MARK: {
                // either TraitObjectType or TraitObjectTypeOneBound
                bool has_dyn = false;
                if (t->get_id() == DYN) {
                    lexer.skip_token();
                    has_dyn = true;
                }

                if (lexer.peek_token()->get_id() == LIFETIME) {
                    // cannot be one bound because lifetime prevents it from being traitbound
                    ::std::vector< ::std::unique_ptr<AST::TypeParamBound> > bounds
                      = parse_type_param_bounds();

                    return ::std::unique_ptr<AST::TraitObjectType>(
                      new AST::TraitObjectType(::std::move(bounds), t->get_locus(), has_dyn));
                } else {
                    // should be trait bound, so parse trait bound
                    ::std::unique_ptr<AST::TraitBound> initial_bound = parse_trait_bound();
                    if (initial_bound == NULL) {
                        error_at(lexer.peek_token()->get_locus(),
                          "failed to parse TraitObjectType initial bound");
                        return NULL;
                    }

                    // short cut if next token isn't '+'
                    t = lexer.peek_token();
                    if (t->get_id() != PLUS) {
                        // convert trait bound to value object
                        AST::TraitBound value_bound(*initial_bound);

                        // DEBUG: removed as unique ptr, so should auto delete
                        // delete initial_bound;

                        return ::std::unique_ptr<AST::TraitObjectTypeOneBound>(
                          new AST::TraitObjectTypeOneBound(
                            ::std::move(value_bound), t->get_locus(), has_dyn));
                    }

                    // parse additional type param bounds
                    ::std::vector< ::std::unique_ptr<AST::TypeParamBound> > bounds;
                    bounds.push_back(::std::move(initial_bound));
                    while (t->get_id() == PLUS) {
                        lexer.skip_token();

                        // parse bound if it exists
                        ::std::unique_ptr<AST::TypeParamBound> bound = parse_type_param_bound();
                        if (bound == NULL) {
                            // not an error as trailing plus may exist
                            break;
                        }
                        bounds.push_back(::std::move(bound));

                        t = lexer.peek_token();
                    }

                    return ::std::unique_ptr<AST::TraitObjectType>(
                      new AST::TraitObjectType(::std::move(bounds), t->get_locus(), has_dyn));
                }
            }
            default:
                error_at(
                  t->get_locus(), "unrecognised token '%s' in type", t->get_token_description());
                return NULL;
        }
    }

    /* Parses a type that has '(' as its first character. Returns a tuple type, parenthesised type,
     * TraitObjectTypeOneBound, or TraitObjectType depending on following characters. */
    ::std::unique_ptr<AST::Type> Parser::parse_paren_prefixed_type() {
        /* NOTE: Syntactical ambiguity of a parenthesised trait bound is considered a trait bound,
         * not a parenthesised type, so that it can still be used in type param bounds. */

        /* NOTE: this implementation is really shit but I couldn't think of a better one. It requires
         * essentially breaking polymorphism and downcasting via virtual method abuse, as it was
         * copied from the rustc implementation (in which types are reified due to tagged union),
         * after a more OOP attempt by me failed. */
        location_t left_delim_locus = lexer.peek_token()->get_locus();

        // skip left delim
        lexer.skip_token();
        // while next token isn't close delim, parse comma-separated types, saving whether trailing
        // comma happens
        const_TokenPtr t = lexer.peek_token();
        bool trailing_comma = true;
        ::std::vector< ::std::unique_ptr<AST::Type> > types;

        while (t->get_id() != RIGHT_PAREN) {
            ::std::unique_ptr<AST::Type> type = parse_type();
            if (type == NULL) {
                error_at(t->get_locus(),
                  "failed to parse type inside parentheses (probably tuple or parenthesised)");
                return NULL;
            }
            types.push_back(::std::move(type));

            t = lexer.peek_token();
            if (t->get_id() != COMMA) {
                trailing_comma = false;
                break;
            }
            lexer.skip_token();

            t = lexer.peek_token();
        }

        if (!skip_token(RIGHT_PAREN)) {
            return NULL;
        }

        // if only one type and no trailing comma, then not a tuple type
        if (types.size() == 1 && !trailing_comma) {
            // must be a TraitObjectType (with more than one bound)
            if (lexer.peek_token()->get_id() == PLUS) {
                // create type param bounds vector
                ::std::vector< ::std::unique_ptr<AST::TypeParamBound> > bounds;

                // HACK: convert type to traitbound and add to bounds
                AST::Type* released_ptr = types[0].release();
                AST::TraitBound* converted_bound = released_ptr->to_trait_bound(true);
                delete released_ptr;
                if (converted_bound == NULL) {
                    error_at(lexer.peek_token()->get_id(),
                      "failed to hackily converted parsed type to trait bound");
                    return NULL;
                }
                bounds.push_back(::std::unique_ptr<AST::TraitBound>(converted_bound));
                // FIXME: possibly issues wrt memory here

                t = lexer.peek_token();
                while (t->get_id() == PLUS) {
                    lexer.skip_token();

                    // attempt to parse typeparambound
                    ::std::unique_ptr<AST::TypeParamBound> bound = parse_type_param_bound();
                    if (bound == NULL) {
                        // not an error if null
                        break;
                    }
                    bounds.push_back(::std::move(bound));

                    t = lexer.peek_token();
                }

                return ::std::unique_ptr<AST::TraitObjectType>(
                  new AST::TraitObjectType(::std::move(bounds), left_delim_locus));
            } else {
                // release vector pointer
                ::std::unique_ptr<AST::Type> released_ptr(types[0].release());
                // HACK: attempt to convert to trait bound. if fails, parenthesised type
                ::std::unique_ptr<AST::TraitBound> converted_bound(
                  released_ptr->to_trait_bound(true));
                if (converted_bound == NULL) {
                    // parenthesised type
                    return ::std::unique_ptr<AST::ParenthesisedType>(
                      new AST::ParenthesisedType(::std::move(released_ptr), left_delim_locus));
                } else {
                    // trait object type (one bound)

                    // DEBUG: removed as unique_ptr should auto-delete
                    // delete released_ptr;

                    // get value semantics trait bound
                    AST::TraitBound value_bound(*converted_bound);

                    // DEBUG: removed as unique ptr should auto-delete
                    // delete converted_bound;

                    return ::std::unique_ptr<AST::TraitObjectTypeOneBound>(
                      new AST::TraitObjectTypeOneBound(value_bound, left_delim_locus));
                }
                // FIXME: may be issues wrt memory here
            }
        } else {
            return ::std::unique_ptr<AST::TupleType>(
              new AST::TupleType(::std::move(types), left_delim_locus));
        }
        // TODO: ensure that this ensures that dynamic dispatch for traits is not lost somehow
    }

    /* Parses a type that has 'for' as its first character. This means it has a "for lifetimes", so
     * returns either a BareFunctionType, TraitObjectType, or TraitObjectTypeOneBound depending on
     * following characters. */
    ::std::unique_ptr<AST::Type> Parser::parse_for_prefixed_type() {
        location_t for_locus = lexer.peek_token()->get_locus();
        // parse for lifetimes in type
        ::std::vector<AST::LifetimeParam> for_lifetimes = parse_for_lifetimes();

        // branch on next token - either function or a trait type
        const_TokenPtr t = lexer.peek_token();
        switch (t->get_id()) {
            case ASYNC:
            case CONST:
            case UNSAFE:
            case EXTERN_TOK:
            case FN_TOK:
                return parse_bare_function_type(::std::move(for_lifetimes));
            case SCOPE_RESOLUTION:
            case IDENTIFIER:
            case SUPER:
            case SELF:
            case SELF_ALIAS:
            case CRATE:
            case DOLLAR_SIGN: {
                // path, so trait type

                // parse type path to finish parsing trait bound
                AST::TypePath path = parse_type_path();

                t = lexer.peek_token();
                if (t->get_id() != PLUS) {
                    // must be one-bound trait type
                    // create trait bound value object
                    AST::TraitBound bound(
                      ::std::move(path), for_locus, false, false, ::std::move(for_lifetimes));

                    return ::std::unique_ptr<AST::TraitObjectTypeOneBound>(
                      new AST::TraitObjectTypeOneBound(::std::move(bound), for_locus));
                }

                // more than one bound trait type (or at least parsed as it - could be trailing '+')
                // create trait bound pointer and bounds
                ::std::unique_ptr<AST::TraitBound> initial_bound(new AST::TraitBound(
                  ::std::move(path), for_locus, false, false, ::std::move(for_lifetimes)));
                ::std::vector< ::std::unique_ptr<AST::TypeParamBound> > bounds;
                bounds.push_back(::std::move(initial_bound));

                while (t->get_id() == PLUS) {
                    lexer.skip_token();

                    // parse type param bound if it exists
                    ::std::unique_ptr<AST::TypeParamBound> bound = parse_type_param_bound();
                    if (bound == NULL) {
                        // not an error - e.g. trailing plus
                        return NULL;
                    }
                    bounds.push_back(::std::move(bound));

                    t = lexer.peek_token();
                }

                return ::std::unique_ptr<AST::TraitObjectType>(
                  new AST::TraitObjectType(::std::move(bounds), for_locus));
            }
            default:
                // error
                error_at(t->get_locus(),
                  "unrecognised token '%s' in bare function type or trait object type or trait "
                  "object type one bound",
                  t->get_token_description());
                return NULL;
        }
    }

    // Parses a maybe named param used in bare function types.
    AST::MaybeNamedParam Parser::parse_maybe_named_param() {
        /* Basically guess that param is named if first token is identifier or underscore and
         * second token is semicolon. This should probably have no exceptions. rustc uses
         * backtracking to parse these, but at the time of writing gccrs has no backtracking
         * capabilities. */
        const_TokenPtr current = lexer.peek_token();
        const_TokenPtr next = lexer.peek_token(1);

        Identifier name;
        AST::MaybeNamedParam::ParamKind kind = AST::MaybeNamedParam::UNNAMED;

        if (current->get_id() == IDENTIFIER && next->get_id() == COLON) {
            // named param
            name = current->get_str();
            kind = AST::MaybeNamedParam::IDENTIFIER;
            lexer.skip_token(1);
        } else if (current->get_id() == UNDERSCORE && next->get_id() == COLON) {
            // wildcard param
            name = "_";
            kind = AST::MaybeNamedParam::WILDCARD;
            lexer.skip_token(1);
        }

        // parse type (required)
        ::std::unique_ptr<AST::Type> type = parse_type();
        if (type == NULL) {
            error_at(lexer.peek_token()->get_locus(), "failed to parse type in maybe named param");
            return AST::MaybeNamedParam::create_error();
        }

        return AST::MaybeNamedParam(::std::move(name), kind, ::std::move(type), current->get_locus());
    }

    /* Parses a bare function type (with the given for lifetimes for convenience - does not parse them
     * itself). */
    ::std::unique_ptr<AST::BareFunctionType> Parser::parse_bare_function_type(
      ::std::vector<AST::LifetimeParam> for_lifetimes) {
        // TODO: pass in for lifetime location as param
        location_t best_try_locus = lexer.peek_token()->get_locus();

        AST::FunctionQualifiers qualifiers = parse_function_qualifiers();

        if (!skip_token(FN_TOK)) {
            return NULL;
        }

        if (!skip_token(LEFT_PAREN)) {
            return NULL;
        }

        // parse function params, if they exist
        ::std::vector<AST::MaybeNamedParam> params;
        bool is_variadic = false;
        const_TokenPtr t = lexer.peek_token();
        while (t->get_id() != RIGHT_PAREN) {
            // handle ellipsis (only if next character is right paren)
            if (t->get_id() == ELLIPSIS) {
                if (lexer.peek_token(1)->get_id() == RIGHT_PAREN) {
                    lexer.skip_token();
                    is_variadic = true;
                    break;
                } else {
                    error_at(t->get_locus(),
                      "ellipsis (for variadic) can only go at end of bare function type");
                    return NULL;
                }
            }

            // parse required param
            AST::MaybeNamedParam param = parse_maybe_named_param();
            if (param.is_error()) {
                error_at(t->get_locus(), "failed to parse maybe named param in bare function type");
                return NULL;
            }
            params.push_back(::std::move(param));

            if (lexer.peek_token()->get_id() != COMMA) {
                break;
            }
            lexer.skip_token();

            t = lexer.peek_token();
        }

        if (!skip_token(RIGHT_PAREN)) {
            return NULL;
        }

        // bare function return type, if exists
        ::std::unique_ptr<AST::TypeNoBounds> return_type = NULL;
        if (lexer.peek_token()->get_id() == RETURN_TYPE) {
            lexer.skip_token();

            // parse required TypeNoBounds
            return_type = parse_type_no_bounds();
            if (return_type == NULL) {
                error_at(lexer.peek_token()->get_locus(),
                  "failed to parse return type (type no bounds) in bare function type");
                return NULL;
            }
        }

        return ::std::unique_ptr<AST::BareFunctionType>(
          new AST::BareFunctionType(::std::move(for_lifetimes), ::std::move(qualifiers),
            ::std::move(params), is_variadic, ::std::move(return_type), best_try_locus));
    }

    // Parses a reference type (mutable or immutable, with given lifetime).
    ::std::unique_ptr<AST::ReferenceType> Parser::parse_reference_type() {
        location_t locus = lexer.peek_token()->get_locus();
        skip_token(AMP);

        // parse optional lifetime
        AST::Lifetime lifetime = AST::Lifetime::error();
        if (lexer.peek_token()->get_id() == LIFETIME) {
            lifetime = parse_lifetime();
            if (lifetime.is_error()) {
                error_at(
                  lexer.peek_token()->get_locus(), "failed to parse lifetime in reference type");
                return NULL;
            }
        }

        bool is_mut = false;
        if (lexer.peek_token()->get_id() == MUT) {
            lexer.skip_token();
            is_mut = true;
        }

        // parse type no bounds, which is required
        ::std::unique_ptr<AST::TypeNoBounds> type = parse_type_no_bounds();
        if (type == NULL) {
            error_at(
              lexer.peek_token()->get_id(), "failed to parse referenced type in reference type");
            return NULL;
        }

        return ::std::unique_ptr<AST::ReferenceType>(
          new AST::ReferenceType(is_mut, ::std::move(type), locus, ::std::move(lifetime)));
    }

    // Parses a raw (unsafe) pointer type.
    ::std::unique_ptr<AST::RawPointerType> Parser::parse_raw_pointer_type() {
        location_t locus = lexer.peek_token()->get_locus();
        skip_token(ASTERISK);

        AST::RawPointerType::PointerType kind = AST::RawPointerType::CONST;

        // branch on next token for pointer kind info
        const_TokenPtr t = lexer.peek_token();
        switch (t->get_id()) {
            case MUT:
                kind = AST::RawPointerType::MUT;
                lexer.skip_token();
                break;
            case CONST:
                kind = AST::RawPointerType::CONST;
                lexer.skip_token();
                break;
            default:
                error_at(t->get_locus(), "unrecognised token '%s' in raw pointer type",
                  t->get_token_description());
                return NULL;
        }

        // parse type no bounds (required)
        ::std::unique_ptr<AST::TypeNoBounds> type = parse_type_no_bounds();
        if (type == NULL) {
            error_at(
              lexer.peek_token()->get_locus(), "failed to parse pointed type of raw pointer type");
            return NULL;
        }

        return ::std::unique_ptr<AST::RawPointerType>(
          new AST::RawPointerType(kind, ::std::move(type), locus));
    }

    // Parses a slice or array type, depending on following arguments (as lookahead is not possible).
    ::std::unique_ptr<AST::TypeNoBounds> Parser::parse_slice_or_array_type() {
        location_t locus = lexer.peek_token()->get_locus();
        skip_token(LEFT_SQUARE);

        // parse inner type (required)
        ::std::unique_ptr<AST::Type> inner_type = parse_type();
        if (inner_type == NULL) {
            error_at(
              lexer.peek_token()->get_locus(), "failed to parse inner type in slice or array type");
            return NULL;
        }

        // branch on next token
        const_TokenPtr t = lexer.peek_token();
        switch (t->get_id()) {
            case RIGHT_SQUARE:
                // slice type
                lexer.skip_token();

                return ::std::unique_ptr<AST::SliceType>(
                  new AST::SliceType(::std::move(inner_type), locus));
            case SEMICOLON: {
                // array type
                lexer.skip_token();

                // parse required array size expression
                ::std::unique_ptr<AST::Expr> size = parse_expr();
                if (size == NULL) {
                    error_at(lexer.peek_token()->get_locus(),
                      "failed to parse size expression in array type");
                    return NULL;
                }

                if (!skip_token(RIGHT_SQUARE)) {
                    return NULL;
                }

                return ::std::unique_ptr<AST::ArrayType>(
                  new AST::ArrayType(::std::move(inner_type), ::std::move(size), locus));
            }
            default:
                // error
                error_at(t->get_locus(),
                  "unrecognised token '%s' in slice or array type after inner type",
                  t->get_token_description());
                return NULL;
        }
    }

    // Parses a type, taking into account type boundary disambiguation.
    ::std::unique_ptr<AST::TypeNoBounds> Parser::parse_type_no_bounds() {
        const_TokenPtr t = lexer.peek_token();
        switch (t->get_id()) {
            case EXCLAM:
                // never type - can't be macro as no path beforehand
                lexer.skip_token();
                return ::std::unique_ptr<AST::NeverType>(new AST::NeverType(t->get_locus()));
            case LEFT_SQUARE:
                // slice type or array type - requires further disambiguation
                return parse_slice_or_array_type();
            case LEFT_ANGLE: {
                // qualified path in type
                AST::QualifiedPathInType path = parse_qualified_path_in_type();
                if (path.is_error()) {
                    error_at(t->get_locus(), "failed to parse qualified path in type");
                    return NULL;
                }
                return ::std::unique_ptr<AST::QualifiedPathInType>(
                  new AST::QualifiedPathInType(::std::move(path)));
            }
            case UNDERSCORE:
                // inferred type
                lexer.skip_token();
                return ::std::unique_ptr<AST::InferredType>(new AST::InferredType(t->get_locus()));
            case ASTERISK:
                // raw pointer type
                return parse_raw_pointer_type();
            case AMP: // does this also include AMP_AMP?
                // reference type
                return parse_reference_type();
            case LIFETIME: {
                // probably a lifetime bound, so probably type param bounds in TraitObjectType
                // this is not allowed, but detection here for error message
                error_at(t->get_locus(), "lifetime bounds (i.e. in type param bounds, in "
                                         "TraitObjectType) are not allowed as TypeNoBounds");
                return NULL;
            }
            case IDENTIFIER:
            case SUPER:
            case SELF:
            case SELF_ALIAS:
            case CRATE:
            case DOLLAR_SIGN:
            case SCOPE_RESOLUTION: {
                // macro invocation or type path - requires further disambiguation.
                /* for parsing path component of each rule, perhaps parse it as a typepath and
                 * attempt conversion to simplepath if a trailing '!' is found */
                /* Type path also includes TraitObjectTypeOneBound BUT if it starts with it, it is
                 * exactly the same as a TypePath syntactically, so this is a syntactical ambiguity.
                 * As such, the parser will parse it as a TypePath.
                 * This, however, does not prevent TraitObjectType from starting with a typepath. */

                // parse path as type path
                AST::TypePath path = parse_type_path();
                if (path.is_error()) {
                    error_at(
                      t->get_locus(), "failed to parse path as first component of type no bounds");
                    return NULL;
                }
                location_t locus = path.get_locus();

                // branch on next token
                t = lexer.peek_token();
                switch (t->get_id()) {
                    case EXCLAM: {
                        // macro invocation
                        // convert to simple path
                        AST::SimplePath macro_path = path.as_simple_path();
                        if (macro_path.is_empty()) {
                            error_at(t->get_locus(),
                              "failed to parse simple path in macro invocation (for type)");
                            return NULL;
                        }

                        lexer.skip_token();

                        AST::DelimTokenTree tok_tree = parse_delim_token_tree();

                        return ::std::unique_ptr<AST::MacroInvocation>(
                          new AST::MacroInvocation(::std::move(macro_path), ::std::move(tok_tree),
                            ::std::vector<AST::Attribute>(), locus));
                    }
                    case PLUS: {
                        // type param bounds - not allowed, here for error message
                        error_at(t->get_locus(),
                          "type param bounds (in TraitObjectType) are not allowed as TypeNoBounds");
                        return NULL;
                    }
                    default:
                        // assume that this is a type path and not an error
                        return ::std::unique_ptr<AST::TypePath>(new AST::TypePath(::std::move(path)));
                }
            }
            case LEFT_PAREN:
                // tuple type or parenthesised type - requires further disambiguation (the usual)
                // ok apparently can be a parenthesised TraitBound too, so could be
                // TraitObjectTypeOneBound
                return parse_paren_prefixed_type_no_bounds();
            case FOR:
            case ASYNC:
            case CONST:
            case UNSAFE:
            case EXTERN_TOK:
            case FN_TOK:
                // bare function type (with no for lifetimes)
                return parse_bare_function_type(::std::vector<AST::LifetimeParam>());
            case IMPL:
                lexer.skip_token();
                if (lexer.peek_token()->get_id() == LIFETIME) {
                    // cannot be one bound because lifetime prevents it from being traitbound
                    // not allowed as type no bounds, only here for error message
                    error_at(lexer.peek_token()->get_locus(),
                      "lifetime (probably lifetime bound, in type param bounds, in ImplTraitType) is "
                      "not allowed in TypeNoBounds");
                    return NULL;
                } else {
                    // should be trait bound, so parse trait bound
                    ::std::unique_ptr<AST::TraitBound> initial_bound = parse_trait_bound();
                    if (initial_bound == NULL) {
                        error_at(lexer.peek_token()->get_locus(),
                          "failed to parse ImplTraitTypeOneBound bound");
                        return NULL;
                    }

                    location_t locus = t->get_locus();

                    // ensure not a trait with multiple bounds
                    t = lexer.peek_token();
                    if (t->get_id() == PLUS) {
                        error_at(t->get_locus(), "plus after trait bound means an ImplTraitType, "
                                                 "which is not allowed as a TypeNoBounds");
                        return NULL;
                    }

                    // convert trait bound to value object
                    AST::TraitBound value_bound(*initial_bound);

                    // DEBUG: removed as unique ptr so should auto-delete
                    // delete initial_bound;

                    return ::std::unique_ptr<AST::ImplTraitTypeOneBound>(
                      new AST::ImplTraitTypeOneBound(::std::move(value_bound), locus));
                }
            case DYN:
            case QUESTION_MARK: {
                // either TraitObjectTypeOneBound
                bool has_dyn = false;
                if (t->get_id() == DYN) {
                    lexer.skip_token();
                    has_dyn = true;
                }

                if (lexer.peek_token()->get_id() == LIFETIME) {
                    // means that cannot be TraitObjectTypeOneBound - so here for error message
                    error_at(lexer.peek_token()->get_locus(),
                      "lifetime as bound in TraitObjectTypeOneBound "
                      "is not allowed, so cannot be TypeNoBounds");
                    return NULL;
                }

                // should be trait bound, so parse trait bound
                ::std::unique_ptr<AST::TraitBound> initial_bound = parse_trait_bound();
                if (initial_bound == NULL) {
                    error_at(lexer.peek_token()->get_locus(),
                      "failed to parse TraitObjectTypeOneBound initial bound");
                    return NULL;
                }

                location_t locus = t->get_locus();

                // detect error with plus as next token
                t = lexer.peek_token();
                if (t->get_id() == PLUS) {
                    error_at(t->get_locus(), "plus after trait bound means a TraitObjectType, "
                                             "which is not allowed as a TypeNoBounds");
                    return NULL;
                }

                // convert trait bound to value object
                AST::TraitBound value_bound(*initial_bound);

                // DEBUG: removed as unique ptr so should auto delete
                // delete initial_bound;

                return ::std::unique_ptr<AST::TraitObjectTypeOneBound>(
                  new AST::TraitObjectTypeOneBound(::std::move(value_bound), locus, has_dyn));
            }
            default:
                error_at(t->get_locus(), "unrecognised token '%s' in type no bounds",
                  t->get_token_description());
                return NULL;
        }
    }

    // Parses a type no bounds beginning with '('.
    ::std::unique_ptr<AST::TypeNoBounds> Parser::parse_paren_prefixed_type_no_bounds() {
        /* NOTE: this could probably be parsed without the HACK solution of parse_paren_prefixed_type,
         * but I was lazy. So FIXME for future.*/

        /* NOTE: again, syntactical ambiguity of a parenthesised trait bound is considered a trait
         * bound, not a parenthesised type, so that it can still be used in type param bounds. */

        location_t left_paren_locus = lexer.peek_token()->get_locus();

        // skip left delim
        lexer.skip_token();
        // while next token isn't close delim, parse comma-separated types, saving whether trailing
        // comma happens
        const_TokenPtr t = lexer.peek_token();
        bool trailing_comma = true;
        ::std::vector< ::std::unique_ptr<AST::Type> > types;

        while (t->get_id() != RIGHT_PAREN) {
            ::std::unique_ptr<AST::Type> type = parse_type();
            if (type == NULL) {
                error_at(t->get_locus(),
                  "failed to parse type inside parentheses (probably tuple or parenthesised)");
                return NULL;
            }
            types.push_back(::std::move(type));

            t = lexer.peek_token();
            if (t->get_id() != COMMA) {
                trailing_comma = false;
                break;
            }
            lexer.skip_token();

            t = lexer.peek_token();
        }

        if (!skip_token(RIGHT_PAREN)) {
            return NULL;
        }

        // if only one type and no trailing comma, then not a tuple type
        if (types.size() == 1 && !trailing_comma) {
            // must be a TraitObjectType (with more than one bound)
            if (lexer.peek_token()->get_id() == PLUS) {
                // error - this is not allowed for type no bounds
                error_at(lexer.peek_token()->get_id(), "plus (implying TraitObjectType as type param "
                                                       "bounds) is not allowed in type no bounds");
                return NULL;
            } else {
                // release vector pointer
                ::std::unique_ptr<AST::Type> released_ptr(types[0].release());
                // HACK: attempt to convert to trait bound. if fails, parenthesised type
                ::std::unique_ptr<AST::TraitBound> converted_bound(
                  released_ptr->to_trait_bound(true));
                if (converted_bound == NULL) {
                    // parenthesised type
                    return ::std::unique_ptr<AST::ParenthesisedType>(
                      new AST::ParenthesisedType(::std::move(released_ptr), left_paren_locus));
                } else {
                    // trait object type (one bound)

                    // DEBUG: removed as unique_ptr should auto-delete
                    // delete released_ptr;

                    // get value semantics trait bound
                    AST::TraitBound value_bound(*converted_bound);

                    // DEBUG: removed as unique_ptr should auto-delete
                    // delete converted_bound;

                    return ::std::unique_ptr<AST::TraitObjectTypeOneBound>(
                      new AST::TraitObjectTypeOneBound(value_bound, left_paren_locus));
                }
                // FIXME: memory safety issues here
            }
        } else {
            return ::std::unique_ptr<AST::TupleType>(
              new AST::TupleType(::std::move(types), left_paren_locus));
        }
        // TODO: ensure that this ensures that dynamic dispatch for traits is not lost somehow
    }

    /* Parses a literal pattern or range pattern. Assumes that literals passed in are valid range
     * pattern bounds. Do not pass in paths in expressions, for instance. */
    ::std::unique_ptr<AST::Pattern> Parser::parse_literal_or_range_pattern() {
        const_TokenPtr range_lower = lexer.peek_token();
        AST::Literal::LitType type = AST::Literal::STRING;
        bool has_minus = false;

        // get lit type
        switch (range_lower->get_id()) {
            case CHAR_LITERAL:
                type = AST::Literal::CHAR;
                lexer.skip_token();
                break;
            case BYTE_CHAR_LITERAL:
                type = AST::Literal::BYTE;
                lexer.skip_token();
                break;
            case INT_LITERAL:
                type = AST::Literal::INT;
                lexer.skip_token();
                break;
            case FLOAT_LITERAL:
                type = AST::Literal::FLOAT;
                lexer.skip_token();
                break;
            case MINUS:
                // branch on next token
                range_lower = lexer.peek_token(1);
                switch (range_lower->get_id()) {
                    case INT_LITERAL:
                        type = AST::Literal::INT;
                        has_minus = true;
                        lexer.skip_token(1);
                        break;
                    case FLOAT_LITERAL:
                        type = AST::Literal::FLOAT;
                        has_minus = true;
                        lexer.skip_token(1);
                        break;
                    default:
                        error_at(range_lower->get_locus(),
                          "token type '%s' cannot be parsed as range pattern bound or literal after "
                          "minus "
                          "symbol",
                          range_lower->get_token_description());
                        return NULL;
                }
                break;
            default:
                error_at(range_lower->get_locus(),
                  "token type '%s' cannot be parsed as range pattern bound",
                  range_lower->get_token_description());
                return NULL;
        }

        const_TokenPtr next = lexer.peek_token();
        if (next->get_id() == DOT_DOT_EQ || next->get_id() == ELLIPSIS) {
            // range pattern
            lexer.skip_token();
            ::std::unique_ptr<AST::RangePatternBound> lower(new AST::RangePatternBoundLiteral(
              AST::Literal(range_lower->get_str(), type), range_lower->get_locus(), has_minus));

            ::std::unique_ptr<AST::RangePatternBound> upper = parse_range_pattern_bound();
            if (upper == NULL) {
                error_at(next->get_locus(), "failed to parse range pattern bound in range pattern");
                return NULL;
            }

            return ::std::unique_ptr<AST::RangePattern>(new AST::RangePattern(
              ::std::move(lower), ::std::move(upper), range_lower->get_locus()));
        } else {
            // literal pattern
            return ::std::unique_ptr<AST::LiteralPattern>(new AST::LiteralPattern(
              range_lower->get_str(), type, range_lower->get_locus(), has_minus));
        }
    }

    // Parses a range pattern bound (value only).
    ::std::unique_ptr<AST::RangePatternBound> Parser::parse_range_pattern_bound() {
        const_TokenPtr range_lower = lexer.peek_token();
        location_t range_lower_locus = range_lower->get_locus();

        // get lit type
        switch (range_lower->get_id()) {
            case CHAR_LITERAL:
                lexer.skip_token();
                return ::std::unique_ptr<AST::RangePatternBoundLiteral>(
                  new AST::RangePatternBoundLiteral(
                    AST::Literal(range_lower->get_str(), AST::Literal::CHAR), range_lower_locus));
            case BYTE_CHAR_LITERAL:
                lexer.skip_token();
                return ::std::unique_ptr<AST::RangePatternBoundLiteral>(
                  new AST::RangePatternBoundLiteral(
                    AST::Literal(range_lower->get_str(), AST::Literal::BYTE), range_lower_locus));
            case INT_LITERAL:
                lexer.skip_token();
                return ::std::unique_ptr<AST::RangePatternBoundLiteral>(
                  new AST::RangePatternBoundLiteral(
                    AST::Literal(range_lower->get_str(), AST::Literal::INT), range_lower_locus));
            case FLOAT_LITERAL:
                lexer.skip_token();
                fprintf(stderr, "warning: used deprecated float range pattern bound");
                return ::std::unique_ptr<AST::RangePatternBoundLiteral>(
                  new AST::RangePatternBoundLiteral(
                    AST::Literal(range_lower->get_str(), AST::Literal::FLOAT), range_lower_locus));
            case MINUS:
                // branch on next token
                range_lower = lexer.peek_token(1);
                switch (range_lower->get_id()) {
                    case INT_LITERAL:
                        lexer.skip_token(1);
                        return ::std::unique_ptr<AST::RangePatternBoundLiteral>(
                          new AST::RangePatternBoundLiteral(
                            AST::Literal(range_lower->get_str(), AST::Literal::INT),
                            range_lower_locus, true));
                    case FLOAT_LITERAL:
                        lexer.skip_token(1);
                        fprintf(stderr, "warning: used deprecated float range pattern bound");
                        return ::std::unique_ptr<AST::RangePatternBoundLiteral>(
                          new AST::RangePatternBoundLiteral(
                            AST::Literal(range_lower->get_str(), AST::Literal::FLOAT),
                            range_lower_locus, true));
                    default:
                        error_at(range_lower->get_locus(),
                          "token type '%s' cannot be parsed as range pattern bound after minus "
                          "symbol",
                          range_lower->get_token_description());
                        return NULL;
                }
            case IDENTIFIER:
            case SUPER:
            case SELF:
            case SELF_ALIAS:
            case CRATE:
            case SCOPE_RESOLUTION:
            case DOLLAR_SIGN: {
                // path in expression
                AST::PathInExpression path = parse_path_in_expression();
                if (path.is_error()) {
                    error_at(range_lower->get_locus(),
                      "failed to parse path in expression range pattern bound");
                    return NULL;
                }
                return ::std::unique_ptr<AST::RangePatternBoundPath>(
                  new AST::RangePatternBoundPath(::std::move(path)));
            }
            case LEFT_ANGLE: {
                // qualified path in expression
                AST::QualifiedPathInExpression path = parse_qualified_path_in_expression();
                if (path.is_error()) {
                    error_at(range_lower->get_locus(),
                      "failed to parse qualified path in expression range pattern bound");
                    return NULL;
                }
                return ::std::unique_ptr<AST::RangePatternBoundQualPath>(
                  new AST::RangePatternBoundQualPath(::std::move(path)));
            }
            default:
                error_at(range_lower->get_locus(),
                  "token type '%s' cannot be parsed as range pattern bound",
                  range_lower->get_token_description());
                return NULL;
        }
    }

    // Parses a pattern (will further disambiguate any pattern).
    ::std::unique_ptr<AST::Pattern> Parser::parse_pattern() {
        const_TokenPtr t = lexer.peek_token();
        switch (t->get_id()) {
            case TRUE_LITERAL:
                lexer.skip_token();
                return ::std::unique_ptr<AST::LiteralPattern>(
                  new AST::LiteralPattern("true", AST::Literal::BOOL, t->get_locus()));
            case FALSE_LITERAL:
                lexer.skip_token();
                return ::std::unique_ptr<AST::LiteralPattern>(
                  new AST::LiteralPattern("false", AST::Literal::BOOL, t->get_locus()));
            case CHAR_LITERAL:
            case BYTE_CHAR_LITERAL:
            case INT_LITERAL:
            case FLOAT_LITERAL:
                return parse_literal_or_range_pattern();
            case STRING_LITERAL:
                lexer.skip_token();
                return ::std::unique_ptr<AST::LiteralPattern>(
                  new AST::LiteralPattern(t->get_str(), AST::Literal::STRING, t->get_locus()));
            case BYTE_STRING_LITERAL:
                lexer.skip_token();
                return ::std::unique_ptr<AST::LiteralPattern>(
                  new AST::LiteralPattern(t->get_str(), AST::Literal::BYTE_STRING, t->get_locus()));
            // raw string and raw byte string literals too if they are readded to lexer
            case MINUS:
                if (lexer.peek_token(1)->get_id() == INT_LITERAL) {
                    return parse_literal_or_range_pattern();
                } else if (lexer.peek_token(1)->get_id() == FLOAT_LITERAL) {
                    return parse_literal_or_range_pattern();
                } else {
                    error_at(t->get_locus(),
                      "unexpected token '-' in pattern - did you forget an integer literal?");
                    return NULL;
                }
            case UNDERSCORE:
                lexer.skip_token();
                return ::std::unique_ptr<AST::WildcardPattern>(
                  new AST::WildcardPattern(t->get_locus()));
            case REF:
            case MUT:
                return parse_identifier_pattern();
            case IDENTIFIER:
                // if identifier with no scope resolution afterwards, identifier pattern.
                // if scope resolution afterwards, path pattern (or range pattern or struct pattern or
                // tuple struct pattern) or macro invocation
                return parse_ident_leading_pattern();
            case AMP:
            case LOGICAL_AND:
                // reference pattern
                return parse_reference_pattern();
            case LEFT_PAREN:
                // tuple pattern or grouped pattern
                return parse_grouped_or_tuple_pattern();
            case LEFT_SQUARE:
                // slice pattern
                return parse_slice_pattern();
            case LEFT_ANGLE: {
                // qualified path in expression or qualified range pattern bound
                AST::QualifiedPathInExpression path = parse_qualified_path_in_expression();

                if (lexer.peek_token()->get_id() == DOT_DOT_EQ
                    || lexer.peek_token()->get_id() == ELLIPSIS) {
                    // qualified range pattern bound, so parse rest of range pattern
                    bool has_ellipsis_syntax = lexer.peek_token()->get_id() == ELLIPSIS;
                    lexer.skip_token();

                    ::std::unique_ptr<AST::RangePatternBoundQualPath> lower_bound(
                      new AST::RangePatternBoundQualPath(::std::move(path)));
                    ::std::unique_ptr<AST::RangePatternBound> upper_bound
                      = parse_range_pattern_bound();

                    return ::std::unique_ptr<AST::RangePattern>(
                      new AST::RangePattern(::std::move(lower_bound), ::std::move(upper_bound),
                        t->get_locus(), has_ellipsis_syntax));
                } else {
                    // just qualified path in expression
                    return ::std::unique_ptr<AST::QualifiedPathInExpression>(
                      new AST::QualifiedPathInExpression(::std::move(path)));
                }
            }
            case SUPER:
            case SELF:
            case SELF_ALIAS:
            case CRATE:
            case SCOPE_RESOLUTION:
            case DOLLAR_SIGN: {
                // path in expression or range pattern bound
                AST::PathInExpression path = parse_path_in_expression();

                const_TokenPtr next = lexer.peek_token();
                switch (next->get_id()) {
                    case DOT_DOT_EQ:
                    case ELLIPSIS: {
                        // qualified range pattern bound, so parse rest of range pattern
                        bool has_ellipsis_syntax = lexer.peek_token()->get_id() == ELLIPSIS;
                        lexer.skip_token();

                        ::std::unique_ptr<AST::RangePatternBoundPath> lower_bound(
                          new AST::RangePatternBoundPath(::std::move(path)));
                        ::std::unique_ptr<AST::RangePatternBound> upper_bound
                          = parse_range_pattern_bound();

                        return ::std::unique_ptr<AST::RangePattern>(new AST::RangePattern(
                          ::std::move(lower_bound), ::std::move(upper_bound), has_ellipsis_syntax));
                    }
                    case EXCLAM:
                        return parse_macro_invocation_partial(
                          ::std::move(path), ::std::vector<AST::Attribute>());
                    case LEFT_PAREN: {
                        // tuple struct
                        lexer.skip_token();

                        // parse items
                        ::std::unique_ptr<AST::TupleStructItems> items = parse_tuple_struct_items();
                        if (items == NULL) {
                            error_at(
                              lexer.peek_token()->get_locus(), "failed to parse tuple struct items");
                            return NULL;
                        }

                        if (!skip_token(RIGHT_PAREN)) {
                            return NULL;
                        }

                        return ::std::unique_ptr<AST::TupleStructPattern>(
                          new AST::TupleStructPattern(::std::move(path), ::std::move(items)));
                    }
                    case LEFT_CURLY: {
                        // struct
                        lexer.skip_token();

                        // parse elements (optional)
                        AST::StructPatternElements elems = parse_struct_pattern_elems();

                        if (!skip_token(RIGHT_CURLY)) {
                            return NULL;
                        }

                        return ::std::unique_ptr<AST::StructPattern>(
                          new AST::StructPattern(::std::move(path), ::std::move(elems)));
                    }
                    default:
                        // assume path in expression
                        return ::std::unique_ptr<AST::PathInExpression>(
                          new AST::PathInExpression(::std::move(path)));
                }
            }
            default:
                error_at(
                  t->get_locus(), "unexpected token '%s' in pattern", t->get_token_description());
                return NULL;
        }
    }

    // Parses a single or double reference pattern.
    ::std::unique_ptr<AST::ReferencePattern> Parser::parse_reference_pattern() {
        // parse double or single ref
        bool is_double_ref = false;
        const_TokenPtr t = lexer.peek_token();
        switch (t->get_id()) {
            case AMP:
                // still false
                lexer.skip_token();
                break;
            case LOGICAL_AND:
                is_double_ref = true;
                lexer.skip_token();
                break;
            default:
                error_at(t->get_locus(), "unexpected token '%s' in reference pattern",
                  t->get_token_description());
                return NULL;
        }

        // parse mut (if it exists)
        bool is_mut = false;
        if (lexer.peek_token()->get_id() == MUT) {
            is_mut = true;
            lexer.skip_token();
        }

        // parse pattern to get reference of (required)
        ::std::unique_ptr<AST::Pattern> pattern = parse_pattern();
        if (pattern == NULL) {
            error_at(lexer.peek_token()->get_locus(), "failed to parse pattern in reference pattern");
            // skip somewhere?
            return NULL;
        }

        return ::std::unique_ptr<AST::ReferencePattern>(
          new AST::ReferencePattern(::std::move(pattern), is_mut, is_double_ref, t->get_locus()));
    }

    /* Parses a grouped pattern or tuple pattern. Prefers grouped over tuple if only a single element
     * with no commas. */
    ::std::unique_ptr<AST::Pattern> Parser::parse_grouped_or_tuple_pattern() {
        location_t paren_locus = lexer.peek_token()->get_locus();
        skip_token(LEFT_PAREN);

        // detect '..' token (ranged with no lower range)
        if (lexer.peek_token()->get_id() == DOT_DOT) {
            lexer.skip_token();

            // parse new patterns while next token is a comma
            ::std::vector< ::std::unique_ptr<AST::Pattern> > patterns;

            const_TokenPtr t = lexer.peek_token();
            while (t->get_id() == COMMA) {
                lexer.skip_token();

                // break if next token is ')'
                if (lexer.peek_token()->get_id() == RIGHT_PAREN) {
                    break;
                }

                // parse pattern, which is required
                ::std::unique_ptr<AST::Pattern> pattern = parse_pattern();
                if (pattern == NULL) {
                    error_at(lexer.peek_token()->get_locus(),
                      "failed to parse pattern inside ranged tuple pattern");
                    // skip somewhere?
                    return NULL;
                }
                patterns.push_back(::std::move(pattern));

                t = lexer.peek_token();
            }

            if (!skip_token(RIGHT_PAREN)) {
                // skip somewhere?
                return NULL;
            }

            // create ranged tuple pattern items with only upper items
            ::std::unique_ptr<AST::TuplePatternItemsRanged> items(new AST::TuplePatternItemsRanged(
              ::std::vector< ::std::unique_ptr<AST::Pattern> >(), ::std::move(patterns)));
            return ::std::unique_ptr<AST::TuplePattern>(
              new AST::TuplePattern(::std::move(items), paren_locus));
        }

        // parse initial pattern (required)
        ::std::unique_ptr<AST::Pattern> initial_pattern = parse_pattern();
        if (initial_pattern == NULL) {
            error_at(
              lexer.peek_token()->get_locus(), "failed to parse pattern in grouped or tuple pattern");
            return NULL;
        }

        // branch on whether next token is a comma or not
        const_TokenPtr t = lexer.peek_token();
        switch (t->get_id()) {
            case RIGHT_PAREN:
                // grouped pattern
                lexer.skip_token();

                return ::std::unique_ptr<AST::GroupedPattern>(
                  new AST::GroupedPattern(::std::move(initial_pattern), paren_locus));
            case COMMA: {
                // tuple pattern
                lexer.skip_token();

                // create vector of patterns
                ::std::vector< ::std::unique_ptr<AST::Pattern> > patterns;
                patterns.push_back(::std::move(initial_pattern));

                t = lexer.peek_token();
                while (t->get_id() != RIGHT_PAREN && t->get_id() != DOT_DOT) {
                    // parse pattern (required)
                    ::std::unique_ptr<AST::Pattern> pattern = parse_pattern();
                    if (pattern == NULL) {
                        error_at(t->get_locus(), "failed to parse pattern in tuple pattern");
                        return NULL;
                    }
                    patterns.push_back(::std::move(pattern));

                    if (lexer.peek_token()->get_id() != COMMA) {
                        break;
                    }
                    lexer.skip_token();

                    t = lexer.peek_token();
                }

                t = lexer.peek_token();
                if (t->get_id() == RIGHT_PAREN) {
                    // non-ranged tuple pattern
                    lexer.skip_token();

                    ::std::unique_ptr<AST::TuplePatternItemsMultiple> items(
                      new AST::TuplePatternItemsMultiple(::std::move(patterns)));
                    return ::std::unique_ptr<AST::TuplePattern>(
                      new AST::TuplePattern(::std::move(items), paren_locus));
                } else if (t->get_id() == DOT_DOT) {
                    // ranged tuple pattern
                    lexer.skip_token();

                    // parse upper patterns
                    ::std::vector< ::std::unique_ptr<AST::Pattern> > upper_patterns;
                    t = lexer.peek_token();
                    while (t->get_id() == COMMA) {
                        lexer.skip_token();

                        // break if end
                        if (lexer.peek_token()->get_id() == RIGHT_PAREN) {
                            break;
                        }

                        // parse pattern (required)
                        ::std::unique_ptr<AST::Pattern> pattern = parse_pattern();
                        if (pattern == NULL) {
                            error_at(lexer.peek_token()->get_locus(),
                              "failed to parse pattern in tuple pattern");
                            return NULL;
                        }
                        upper_patterns.push_back(::std::move(pattern));

                        t = lexer.peek_token();
                    }

                    if (!skip_token(RIGHT_PAREN)) {
                        return NULL;
                    }

                    ::std::unique_ptr<AST::TuplePatternItemsRanged> items(
                      new AST::TuplePatternItemsRanged(
                        ::std::move(patterns), ::std::move(upper_patterns)));
                    return ::std::unique_ptr<AST::TuplePattern>(
                      new AST::TuplePattern(::std::move(items), paren_locus));
                } else {
                    // some kind of error
                    error_at(t->get_locus(),
                      "failed to parse tuple pattern (probably) or maybe grouped pattern");
                    return NULL;
                }
            }
            default:
                // error
                error_at(t->get_locus(),
                  "unrecognised token '%s' in grouped or tuple pattern after first pattern",
                  t->get_token_description());
                return NULL;
        }
    }

    // Parses a slice pattern that can match arrays or slices. Parses the square brackets too.
    ::std::unique_ptr<AST::SlicePattern> Parser::parse_slice_pattern() {
        location_t square_locus = lexer.peek_token()->get_locus();
        skip_token(LEFT_SQUARE);

        // parse initial pattern (required)
        ::std::unique_ptr<AST::Pattern> initial_pattern = parse_pattern();
        if (initial_pattern == NULL) {
            error_at(
              lexer.peek_token()->get_locus(), "failed to parse initial pattern in slice pattern");
            return NULL;
        }

        ::std::vector< ::std::unique_ptr<AST::Pattern> > patterns;
        patterns.push_back(::std::move(initial_pattern));

        const_TokenPtr t = lexer.peek_token();
        while (t->get_id() == COMMA) {
            lexer.skip_token();

            // break if end bracket
            if (lexer.peek_token()->get_id() == RIGHT_SQUARE) {
                break;
            }

            // parse pattern (required)
            ::std::unique_ptr<AST::Pattern> pattern = parse_pattern();
            if (pattern == NULL) {
                error_at(lexer.peek_token()->get_locus(), "failed to parse pattern in slice pattern");
                return NULL;
            }
            patterns.push_back(::std::move(pattern));

            t = lexer.peek_token();
        }

        if (!skip_token(RIGHT_SQUARE)) {
            return NULL;
        }

        return ::std::unique_ptr<AST::SlicePattern>(
          new AST::SlicePattern(::std::move(patterns), square_locus));
    }

    // Parses an identifier pattern (pattern that binds a value matched to a variable).
    ::std::unique_ptr<AST::IdentifierPattern> Parser::parse_identifier_pattern() {
        location_t locus = lexer.peek_token()->get_locus();

        bool has_ref = false;
        if (lexer.peek_token()->get_id() == REF) {
            has_ref = true;
            lexer.skip_token();

            // DEBUG
            fprintf(stderr, "parsed ref in identifier pattern\n");
        }

        bool has_mut = false;
        if (lexer.peek_token()->get_id() == MUT) {
            has_mut = true;
            lexer.skip_token();
        }

        // parse identifier (required)
        const_TokenPtr ident_tok = expect_token(IDENTIFIER);
        if (ident_tok == NULL) {
            // skip somewhere?
            return NULL;
        }
        Identifier ident = ident_tok->get_str();

        // DEBUG
        fprintf(stderr, "parsed identifier in identifier pattern\n");

        // parse optional pattern binding thing
        ::std::unique_ptr<AST::Pattern> bind_pattern = NULL;
        if (lexer.peek_token()->get_id() == PATTERN_BIND) {
            lexer.skip_token();

            // parse required pattern to bind
            bind_pattern = parse_pattern();
            if (bind_pattern == NULL) {
                error_at(lexer.peek_token()->get_locus(),
                  "failed to parse pattern to bind in identifier pattern\n");
                return NULL;
            }
        }

        // DEBUG
        fprintf(stderr, "about to return identifier pattern\n");

        return ::std::unique_ptr<AST::IdentifierPattern>(new AST::IdentifierPattern(
          ::std::move(ident), locus, has_ref, has_mut, ::std::move(bind_pattern)));
    }

    /* Parses a pattern that opens with an identifier. This includes identifier patterns, path
     * patterns (and derivatives such as struct patterns, tuple struct patterns, and macro
     * invocations), and ranges. */
    ::std::unique_ptr<AST::Pattern> Parser::parse_ident_leading_pattern() {
        // ensure first token is actually identifier
        const_TokenPtr initial_tok = lexer.peek_token();
        if (initial_tok->get_id() != IDENTIFIER) {
            return NULL;
        }

        // save initial identifier as it may be useful (but don't skip)
        ::std::string initial_ident = initial_tok->get_str();

        // parse next tokens as a PathInExpression
        AST::PathInExpression path = parse_path_in_expression();

        // branch on next token
        const_TokenPtr t = lexer.peek_token();
        switch (t->get_id()) {
            case EXCLAM:
                return parse_macro_invocation_partial(
                  ::std::move(path), ::std::vector<AST::Attribute>());
            case LEFT_PAREN: {
                // tuple struct
                lexer.skip_token();

                // DEBUG
                fprintf(stderr, "parsing tuple struct pattern\n");

                // parse items
                ::std::unique_ptr<AST::TupleStructItems> items = parse_tuple_struct_items();
                if (items == NULL) {
                    error_at(lexer.peek_token()->get_locus(), "failed to parse tuple struct items");
                    return NULL;
                }

                // DEBUG
                fprintf(stderr, "successfully parsed tuple struct items\n");

                if (!skip_token(RIGHT_PAREN)) {
                    return NULL;
                }

                // DEBUG
                fprintf(stderr, "successfully parsed tuple struct pattern\n");

                return ::std::unique_ptr<AST::TupleStructPattern>(
                  new AST::TupleStructPattern(::std::move(path), ::std::move(items)));
            }
            case LEFT_CURLY: {
                // struct
                lexer.skip_token();

                // parse elements (optional)
                AST::StructPatternElements elems = parse_struct_pattern_elems();

                if (!skip_token(RIGHT_CURLY)) {
                    return NULL;
                }

                // DEBUG
                fprintf(stderr, "successfully parsed struct pattern\n");

                return ::std::unique_ptr<AST::StructPattern>(
                  new AST::StructPattern(::std::move(path), ::std::move(elems)));
            }
            case DOT_DOT_EQ:
            case ELLIPSIS: {
                // range
                bool has_ellipsis_syntax = lexer.peek_token()->get_id() == ELLIPSIS;

                lexer.skip_token();

                ::std::unique_ptr<AST::RangePatternBoundPath> lower_bound(
                  new AST::RangePatternBoundPath(::std::move(path)));
                ::std::unique_ptr<AST::RangePatternBound> upper_bound = parse_range_pattern_bound();

                return ::std::unique_ptr<AST::RangePattern>(new AST::RangePattern(
                  ::std::move(lower_bound), ::std::move(upper_bound), has_ellipsis_syntax));
            }
            case PATTERN_BIND: {
                // only allow on single-segment paths
                if (path.is_single_segment()) {
                    // identifier with pattern bind
                    lexer.skip_token();

                    ::std::unique_ptr<AST::Pattern> bind_pattern = parse_pattern();
                    if (bind_pattern == NULL) {
                        error_at(
                          t->get_locus(), "failed to parse pattern to bind to identifier pattern");
                        return NULL;
                    }
                    return ::std::unique_ptr<AST::IdentifierPattern>(
                      new AST::IdentifierPattern(::std::move(initial_ident), initial_tok->get_locus(),
                        false, false, ::std::move(bind_pattern)));
                }
                error_at(t->get_locus(), "failed to parse pattern bind to a path, not an identifier");
                return NULL;
            }
            default:
                // assume identifier if single segment
                if (path.is_single_segment()) {
                    return ::std::unique_ptr<AST::IdentifierPattern>(new AST::IdentifierPattern(
                      ::std::move(initial_ident), initial_tok->get_locus()));
                }
                // return path otherwise
                return ::std::unique_ptr<AST::PathInExpression>(
                  new AST::PathInExpression(::std::move(path)));
        }
    }

    // Parses tuple struct items if they exist. Does not parse parentheses.
    ::std::unique_ptr<AST::TupleStructItems> Parser::parse_tuple_struct_items() {
        ::std::vector< ::std::unique_ptr<AST::Pattern> > lower_patterns;

        // DEBUG
        fprintf(stderr, "started parsing tuple struct items\n");

        // check for '..' at front
        if (lexer.peek_token()->get_id() == DOT_DOT) {
            // only parse upper patterns
            lexer.skip_token();

            // DEBUG
            fprintf(stderr, "'..' at front in tuple struct items detected\n");

            ::std::vector< ::std::unique_ptr<AST::Pattern> > upper_patterns;

            const_TokenPtr t = lexer.peek_token();
            while (t->get_id() == COMMA) {
                lexer.skip_token();

                // break if right paren
                if (lexer.peek_token()->get_id() == RIGHT_PAREN) {
                    break;
                }

                // parse pattern, which is now required
                ::std::unique_ptr<AST::Pattern> pattern = parse_pattern();
                if (pattern == NULL) {
                    error_at(lexer.peek_token()->get_locus(),
                      "failed to parse pattern in tuple struct items");
                    return NULL;
                }
                upper_patterns.push_back(::std::move(pattern));

                t = lexer.peek_token();
            }

            // DEBUG
            fprintf(stderr, "finished parsing tuple struct items ranged (upper/none only)\n");

            return ::std::unique_ptr<AST::TupleStructItemsRange>(new AST::TupleStructItemsRange(
              ::std::move(lower_patterns), ::std::move(upper_patterns)));
        }

        // has at least some lower patterns
        const_TokenPtr t = lexer.peek_token();
        while (t->get_id() != RIGHT_PAREN && t->get_id() != DOT_DOT) {

            // DEBUG
            fprintf(stderr, "about to parse pattern in tuple struct items\n");

            // parse pattern, which is required
            ::std::unique_ptr<AST::Pattern> pattern = parse_pattern();
            if (pattern == NULL) {
                error_at(t->get_locus(), "failed to parse pattern in tuple struct items");
                return NULL;
            }
            lower_patterns.push_back(::std::move(pattern));

            // DEBUG
            fprintf(stderr, "successfully parsed pattern in tuple struct items\n");

            if (lexer.peek_token()->get_id() != COMMA) {
                // DEBUG
                fprintf(stderr, "broke out of parsing patterns in tuple struct items as no comma \n");

                break;
            }
            lexer.skip_token();

            t = lexer.peek_token();
        }

        // branch on next token
        t = lexer.peek_token();
        switch (t->get_id()) {
            case RIGHT_PAREN:
                return ::std::unique_ptr<AST::TupleStructItemsNoRange>(
                  new AST::TupleStructItemsNoRange(::std::move(lower_patterns)));
            case DOT_DOT: {
                // has an upper range that must be parsed separately
                lexer.skip_token();

                ::std::vector< ::std::unique_ptr<AST::Pattern> > upper_patterns;

                t = lexer.peek_token();
                while (t->get_id() == COMMA) {
                    lexer.skip_token();

                    // break if next token is right paren
                    if (lexer.peek_token()->get_id() == RIGHT_PAREN) {
                        break;
                    }

                    // parse pattern, which is required
                    ::std::unique_ptr<AST::Pattern> pattern = parse_pattern();
                    if (pattern == NULL) {
                        error_at(lexer.peek_token()->get_locus(),
                          "failed to parse pattern in tuple struct items");
                        return NULL;
                    }
                    upper_patterns.push_back(::std::move(pattern));

                    t = lexer.peek_token();
                }

                return ::std::unique_ptr<AST::TupleStructItemsRange>(new AST::TupleStructItemsRange(
                  ::std::move(lower_patterns), ::std::move(upper_patterns)));
            }
            default:
                // error
                error_at(t->get_locus(), "unexpected token '%s' in tuple struct items",
                  t->get_token_description());
                return NULL;
        }
    }

    // Parses struct pattern elements if they exist.
    AST::StructPatternElements Parser::parse_struct_pattern_elems() {
        ::std::vector< ::std::unique_ptr<AST::StructPatternField> > fields;

        // try parsing struct pattern fields
        const_TokenPtr t = lexer.peek_token();
        while (t->get_id() != RIGHT_CURLY && t->get_id() != DOT_DOT) {
            ::std::unique_ptr<AST::StructPatternField> field = parse_struct_pattern_field();
            if (field == NULL) {
                // TODO: should this be an error?
                // assuming that this means that it is a struct pattern etc instead

                // DEBUG
                fprintf(stderr, "failed to parse struct pattern field - breaking from loop\n");

                break;
            }

            fields.push_back(::std::move(field));

            // DEBUG
            fprintf(stderr, "successfully pushed back a struct pattern field\n");

            if (lexer.peek_token()->get_id() != COMMA) {
                break;
            }
            lexer.skip_token();

            t = lexer.peek_token();
        }

        // FIXME: this method of parsing prevents parsing any outer attributes on the ..
        // also there seems to be no distinction between having etc and not having etc.
        if (lexer.peek_token()->get_id() == DOT_DOT) {
            lexer.skip_token();

            // as no outer attributes
            AST::StructPatternEtc etc = AST::StructPatternEtc::create_empty();

            return AST::StructPatternElements(::std::move(fields), ::std::move(etc));
        }

        return AST::StructPatternElements(::std::move(fields));
    }

    // Parses a struct pattern field (tuple index/pattern, identifier/pattern, or identifier).
    ::std::unique_ptr<AST::StructPatternField> Parser::parse_struct_pattern_field() {
        // parse outer attributes (if they exist)
        ::std::vector<AST::Attribute> outer_attrs = parse_outer_attributes();

        // branch based on next token
        const_TokenPtr t = lexer.peek_token();
        switch (t->get_id()) {
            case INT_LITERAL: {
                // tuple index
                ::std::string index_str = t->get_str();
                int index = atoi(index_str.c_str());

                if (!skip_token(COLON)) {
                    return NULL;
                }

                // parse required pattern
                ::std::unique_ptr<AST::Pattern> pattern = parse_pattern();
                if (pattern == NULL) {
                    error_at(
                      t->get_locus(), "failed to parse pattern in tuple index struct pattern field");
                    return NULL;
                }

                return ::std::unique_ptr<AST::StructPatternFieldTuplePat>(
                  new AST::StructPatternFieldTuplePat(
                    index, ::std::move(pattern), ::std::move(outer_attrs), t->get_locus()));
            }
            case IDENTIFIER:
                // identifier-pattern OR only identifier
                // branch on next token
                switch (lexer.peek_token(1)->get_id()) {
                    case COLON: {
                        // identifier-pattern
                        Identifier ident = t->get_str();
                        lexer.skip_token();

                        skip_token(COLON);

                        // parse required pattern
                        ::std::unique_ptr<AST::Pattern> pattern = parse_pattern();
                        if (pattern == NULL) {
                            error_at(
                              t->get_locus(), "failed to parse pattern in struct pattern field");
                            return NULL;
                        }

                        return ::std::unique_ptr<AST::StructPatternFieldIdentPat>(
                          new AST::StructPatternFieldIdentPat(::std::move(ident),
                            ::std::move(pattern), ::std::move(outer_attrs), t->get_locus()));
                    }
                    case COMMA:
                    case RIGHT_CURLY: {
                        // identifier only
                        Identifier ident = t->get_str();
                        lexer.skip_token();

                        return ::std::unique_ptr<AST::StructPatternFieldIdent>(
                          new AST::StructPatternFieldIdent(::std::move(ident), false, false,
                            ::std::move(outer_attrs), t->get_locus()));
                    }
                    default:
                        // error
                        error_at(t->get_locus(), "unrecognised token '%s' in struct pattern field",
                          t->get_token_description());
                        return NULL;
                }
            case REF:
            case MUT: {
                // only identifier
                bool has_ref = false;
                if (t->get_id() == REF) {
                    has_ref = true;
                    lexer.skip_token();
                }

                bool has_mut = false;
                if (lexer.peek_token()->get_id() == MUT) {
                    has_mut = true;
                    lexer.skip_token();
                }

                const_TokenPtr ident_tok = expect_token(IDENTIFIER);
                if (ident_tok == NULL) {
                    return NULL;
                }
                Identifier ident = ident_tok->get_str();

                return ::std::unique_ptr<AST::StructPatternFieldIdent>(
                  new AST::StructPatternFieldIdent(
                    ::std::move(ident), has_ref, has_mut, ::std::move(outer_attrs), t->get_locus()));
            }
            default:
                // not necessarily an error
                return NULL;
        }
    }

    /* Parses a statement or expression (depending on whether a trailing semicolon exists). Useful for
     * block expressions where it cannot be determined through lookahead whether it is a statement or
     * expression to be parsed. */
    ExprOrStmt Parser::parse_stmt_or_expr_without_block() {
        // quick exit for empty statement
        const_TokenPtr t = lexer.peek_token();
        if (t->get_id() == SEMICOLON) {
            lexer.skip_token();
            ::std::unique_ptr<AST::EmptyStmt> stmt(new AST::EmptyStmt(t->get_locus()));
            return ExprOrStmt(::std::move(stmt));
        }

        // parse outer attributes
        ::std::vector<AST::Attribute> outer_attrs = parse_outer_attributes();

        // parsing this will be annoying because of the many different possibilities
        /* best may be just to copy paste in parse_item switch, and failing that try to parse outer
         * attributes, and then pass them in to either a let statement or (fallback) expression
         * statement. */
        // FIXME: think of a way to do this without such a large switch?

        /* FIXME: for expressions at least, the only way that they can really be parsed properly in
         * this way is if they don't support operators on them. They must be pratt-parsed otherwise.
         * As such due to composability, only explicit statements will have special cases here. This
         * should roughly correspond to "expr-with-block", but this warning is here in case it isn't
         * the case. */
        t = lexer.peek_token();
        switch (t->get_id()) {
            case LET: {
                // let statement
                ::std::unique_ptr<AST::LetStmt> stmt(parse_let_stmt(::std::move(outer_attrs)));
                return ExprOrStmt(::std::move(stmt));
            }
            case PUB:
            case MOD:
            case EXTERN_TOK:
            case USE:
            case FN_TOK:
            case TYPE:
            case STRUCT_TOK:
            case ENUM_TOK:
            case CONST:
            case STATIC_TOK:
            case TRAIT:
            case IMPL: {
                ::std::unique_ptr<AST::VisItem> item(parse_vis_item(::std::move(outer_attrs)));
                return ExprOrStmt(::std::move(item));
            }
            /* TODO: implement union keyword but not really because of context-dependence
             * crappy hack way to parse a union written below to separate it from the good code. */
            // case UNION:
            case UNSAFE: { // maybe - unsafe traits are a thing
                // if any of these (should be all possible VisItem prefixes), parse a VisItem
                // can't parse item because would require reparsing outer attributes
                const_TokenPtr t2 = lexer.peek_token(1);
                switch (t2->get_id()) {
                    case LEFT_CURLY: {
                        // unsafe block
                        ::std::unique_ptr<AST::ExprStmtWithBlock> stmt(
                          parse_expr_stmt_with_block(::std::move(outer_attrs)));
                        return ExprOrStmt(::std::move(stmt));
                    }
                    case TRAIT: {
                        // unsafe trait
                        ::std::unique_ptr<AST::VisItem> item(
                          parse_vis_item(::std::move(outer_attrs)));
                        return ExprOrStmt(::std::move(item));
                    }
                    case EXTERN_TOK:
                    case FN_TOK: {
                        // unsafe function
                        ::std::unique_ptr<AST::VisItem> item(
                          parse_vis_item(::std::move(outer_attrs)));
                        return ExprOrStmt(::std::move(item));
                    }
                    case IMPL: {
                        // unsafe trait impl
                        ::std::unique_ptr<AST::VisItem> item(
                          parse_vis_item(::std::move(outer_attrs)));
                        return ExprOrStmt(::std::move(item));
                    }
                    default:
                        error_at(t2->get_locus(),
                          "unrecognised token '%s' after parsing unsafe - expected beginning of "
                          "expression or statement",
                          t->get_token_description());
                        // skip somewhere?
                        return ExprOrStmt::create_error();
                }
            }
            case SUPER:
            case SELF:
            case CRATE:
            case DOLLAR_SIGN: {
                /* path-based thing so struct/enum or path or macro invocation of a kind. however, the
                 * expressions are composable (i think) */

                ::std::unique_ptr<AST::ExprWithoutBlock> expr = parse_expr_without_block();

                if (lexer.peek_token()->get_id() == SEMICOLON) {
                    // must be expression statement
                    lexer.skip_token();

                    ::std::unique_ptr<AST::ExprStmtWithoutBlock> stmt(
                      new AST::ExprStmtWithoutBlock(::std::move(expr), t->get_locus()));
                    return ExprOrStmt(::std::move(stmt));
                }

                // return expression
                return ExprOrStmt(::std::move(expr));
            }
                /* FIXME: this is either a macro invocation or macro invocation semi. start parsing to
                 * determine which one it is. */
                // FIXME: or this is another path-based thing - struct/enum or path itself
                // return parse_path_based_stmt_or_expr(::std::move(outer_attrs));
                // FIXME: old code there
            case LOOP:
            case WHILE:
            case FOR:
            case IF:
            case MATCH_TOK:
            case LEFT_CURLY:
            case ASYNC: {
                // all expressions with block, so cannot be final expr without block in function
                ::std::unique_ptr<AST::ExprStmtWithBlock> stmt(
                  parse_expr_stmt_with_block(::std::move(outer_attrs)));
                return ExprOrStmt(::std::move(stmt));
            }
            case LIFETIME: {
                /* FIXME: are there any expressions without blocks that can have lifetime as their
                 * first token? Or is loop expr the only one? */
                // safe side for now:
                const_TokenPtr t2 = lexer.peek_token(2);
                if (lexer.peek_token(1)->get_id() == COLON
                    && (t2->get_id() == LOOP || t2->get_id() == WHILE || t2->get_id() == FOR)) {
                    ::std::unique_ptr<AST::ExprStmtWithBlock> stmt(
                      parse_expr_stmt_with_block(::std::move(outer_attrs)));
                    return ExprOrStmt(::std::move(stmt));
                } else {
                    // should be expr without block
                    ::std::unique_ptr<AST::ExprWithoutBlock> expr = parse_expr_without_block();

                    if (lexer.peek_token()->get_id() == SEMICOLON) {
                        // must be expression statement
                        lexer.skip_token();

                        ::std::unique_ptr<AST::ExprStmtWithoutBlock> stmt(
                          new AST::ExprStmtWithoutBlock(::std::move(expr), t->get_locus()));
                        return ExprOrStmt(::std::move(stmt));
                    }

                    // return expression
                    return ExprOrStmt(::std::move(expr));
                }
            }
            // crappy hack to do union "keyword"
            case IDENTIFIER:
                // TODO: ensure std::string and literal comparison works
                if (t->get_str() == "union") {
                    ::std::unique_ptr<AST::VisItem> item(parse_vis_item(::std::move(outer_attrs)));
                    return ExprOrStmt(::std::move(item));
                    // or should this go straight to parsing union?
                } else if (t->get_str() == "macro_rules") {
                    // macro_rules! macro item
                    ::std::unique_ptr<AST::MacroItem> item(
                      parse_macro_item(::std::move(outer_attrs)));
                    return ExprOrStmt(::std::move(item));
                } else if (lexer.peek_token(1)->get_id() == SCOPE_RESOLUTION
                           || lexer.peek_token(1)->get_id() == EXCLAM
                           || lexer.peek_token(1)->get_id() == LEFT_CURLY) {
                    // path (probably) or macro invocation or struct or enum, so probably a macro
                    // invocation semi decide how to parse - probably parse path and then get macro
                    // from it

                    // FIXME: old code was good until composability was required
                    // return parse_path_based_stmt_or_expr(::std::move(outer_attrs));
                    ::std::unique_ptr<AST::ExprWithoutBlock> expr = parse_expr_without_block();

                    if (lexer.peek_token()->get_id() == SEMICOLON) {
                        // must be expression statement
                        lexer.skip_token();

                        ::std::unique_ptr<AST::ExprStmtWithoutBlock> stmt(
                          new AST::ExprStmtWithoutBlock(::std::move(expr), t->get_locus()));
                        return ExprOrStmt(::std::move(stmt));
                    }

                    // return expression
                    return ExprOrStmt(::std::move(expr));
                }
                gcc_fallthrough();
                // TODO: find out how to disable gcc "implicit fallthrough" warning
            default: {
                // expression statement (without block) or expression itself - parse expression then
                // make it statement if semi afterwards

                ::std::unique_ptr<AST::ExprWithoutBlock> expr = parse_expr_without_block();

                if (lexer.peek_token()->get_id() == SEMICOLON) {
                    // must be expression statement
                    lexer.skip_token();

                    ::std::unique_ptr<AST::ExprStmtWithoutBlock> stmt(
                      new AST::ExprStmtWithoutBlock(::std::move(expr), t->get_locus()));
                    return ExprOrStmt(::std::move(stmt));
                }

                // return expression
                return ExprOrStmt(::std::move(expr));
            }
        }
    }

    // Parses a statement or expression beginning with a path (i.e. macro, struct/enum, or path expr)
    ExprOrStmt Parser::parse_path_based_stmt_or_expr(::std::vector<AST::Attribute> outer_attrs) {
        // attempt to parse path
        location_t stmt_or_expr_loc = lexer.peek_token()->get_locus();
        AST::PathInExpression path = parse_path_in_expression();

        // branch on next token
        const_TokenPtr t2 = lexer.peek_token();
        switch (t2->get_id()) {
            case EXCLAM: {
                // macro invocation or macro invocation semi - depends on whether there is
                // a final ';' convert path in expr to simple path (as that is used in
                // macros)
                AST::SimplePath macro_path = path.as_simple_path();
                if (macro_path.is_empty()) {
                    error_at(t2->get_locus(), "failed to convert parsed path to simple "
                                              "path (for macro invocation or semi)");
                    return ExprOrStmt::create_error();
                }

                // skip exclamation mark
                lexer.skip_token();

                const_TokenPtr t3 = lexer.peek_token();
                location_t tok_tree_loc = t3->get_locus();

                AST::DelimType type = AST::PARENS;
                switch (t3->get_id()) {
                    case LEFT_PAREN:
                        type = AST::PARENS;
                        break;
                    case LEFT_SQUARE:
                        type = AST::SQUARE;
                        break;
                    case LEFT_CURLY:
                        type = AST::CURLY;
                        break;
                    default:
                        error_at(t3->get_locus(),
                          "unrecognised token '%s' in macro invocation - (opening) "
                          "delimiter expected",
                          t3->get_token_description());
                        return ExprOrStmt::create_error();
                }
                lexer.skip_token();

                // parse actual token trees
                ::std::vector< ::std::unique_ptr<AST::TokenTree> > token_trees;

                t3 = lexer.peek_token();
                // parse token trees until the initial delimiter token is found again
                while (!token_id_matches_delims(t3->get_id(), type)) {
                    ::std::unique_ptr<AST::TokenTree> tree = parse_token_tree();

                    if (tree == NULL) {
                        error_at(t3->get_locus(),
                          "failed to parse token tree for macro invocation (or semi) - "
                          "found "
                          "'%s'",
                          t3->get_token_description());
                        return ExprOrStmt::create_error();
                    }

                    token_trees.push_back(::std::move(tree));

                    t3 = lexer.peek_token();
                }

                // parse end delimiters
                t3 = lexer.peek_token();
                if (token_id_matches_delims(t3->get_id(), type)) {
                    // tokens match opening delimiter, so skip.
                    lexer.skip_token();

                    /* with curly bracketed macros, assume it is a macro invocation unless
                     * a semicolon is explicitly put at the end. this is not necessarily
                     * true (i.e. context-dependence) and so may have to be fixed up via
                     * HACKs in semantic
                     * analysis (by checking whether it is the last elem in the vector).
                     */

                    if (lexer.peek_token()->get_id() == SEMICOLON) {
                        lexer.skip_token();

                        ::std::unique_ptr<AST::MacroInvocationSemi> stmt(
                          new AST::MacroInvocationSemi(::std::move(macro_path), type,
                            ::std::move(token_trees), ::std::move(outer_attrs), stmt_or_expr_loc));
                        return ExprOrStmt(::std::move(stmt));
                    }

                    // otherwise, create macro invocation
                    AST::DelimTokenTree delim_tok_tree(type, ::std::move(token_trees), tok_tree_loc);

                    ::std::unique_ptr<AST::MacroInvocation> expr(
                      new AST::MacroInvocation(::std::move(macro_path), ::std::move(delim_tok_tree),
                        ::std::move(outer_attrs), stmt_or_expr_loc));
                    return ExprOrStmt(::std::move(expr));
                } else {
                    // tokens don't match opening delimiters, so produce error
                    error_at(t2->get_locus(),
                      "unexpected token '%s' - expecting closing delimiter '%s' (for a "
                      "macro invocation)",
                      t2->get_token_description(),
                      (type == AST::PARENS ? ")" : (type == AST::SQUARE ? "]" : "}")));
                    return ExprOrStmt::create_error();
                }
            }
            case LEFT_CURLY: {
                /* definitely not a block:
                 *  path '{' ident ','
                 *  path '{' ident ':' [anything] ','
                 *  path '{' ident ':' [not a type]
                 * otherwise, assume block expr and thus path */
                bool not_a_block
                  = lexer.peek_token(1)->get_id() == IDENTIFIER
                    && (lexer.peek_token(2)->get_id() == COMMA
                         || (lexer.peek_token(2)->get_id() == COLON
                              && (lexer.peek_token(4)->get_id() == COMMA
                                   || !can_tok_start_type(lexer.peek_token(3)->get_id()))));
                ::std::unique_ptr<AST::ExprWithoutBlock> expr = NULL;

                if (not_a_block) {
                    // assume struct expr struct (as struct-enum disambiguation requires name
                    // lookup) again, make statement if final ';'
                    expr
                      = parse_struct_expr_struct_partial(::std::move(path), ::std::move(outer_attrs));
                    if (expr == NULL) {
                        error_at(t2->get_locus(), "failed to parse struct expr struct");
                        return ExprOrStmt::create_error();
                    }
                } else {
                    // assume path - make statement if final ';'
                    // lexer.skip_token();

                    // HACK: add outer attrs to path
                    path.replace_outer_attrs(::std::move(outer_attrs));
                    expr = ::std::unique_ptr<AST::PathInExpression>(
                      new AST::PathInExpression(::std::move(path)));
                }

                // determine if statement if ends with semicolon
                if (lexer.peek_token()->get_id() == SEMICOLON) {
                    // statement
                    lexer.skip_token();
                    ::std::unique_ptr<AST::ExprStmtWithoutBlock> stmt(
                      new AST::ExprStmtWithoutBlock(::std::move(expr), stmt_or_expr_loc));
                    return ExprOrStmt(::std::move(stmt));
                }

                // otherwise, expression
                return ExprOrStmt(::std::move(expr));
            }
            case LEFT_PAREN: {
                // assume struct expr tuple (as struct-enum disambiguation requires name
                // lookup) again, make statement if final ';'
                ::std::unique_ptr<AST::StructExprTuple> struct_expr
                  = parse_struct_expr_tuple_partial(::std::move(path), ::std::move(outer_attrs));
                if (struct_expr == NULL) {
                    error_at(t2->get_locus(), "failed to parse struct expr tuple");
                    return ExprOrStmt::create_error();
                }

                // determine if statement if ends with semicolon
                if (lexer.peek_token()->get_id() == SEMICOLON) {
                    // statement
                    lexer.skip_token();
                    ::std::unique_ptr<AST::ExprStmtWithoutBlock> stmt(
                      new AST::ExprStmtWithoutBlock(::std::move(struct_expr), stmt_or_expr_loc));
                    return ExprOrStmt(::std::move(stmt));
                }

                // otherwise, expression
                return ExprOrStmt(::std::move(struct_expr));
            }
            default: {
                // assume path - make statement if final ';'
                // lexer.skip_token();

                // HACK: replace outer attributes in path
                path.replace_outer_attrs(::std::move(outer_attrs));
                ::std::unique_ptr<AST::PathInExpression> expr(
                  new AST::PathInExpression(::std::move(path)));

                if (lexer.peek_token()->get_id() == SEMICOLON) {
                    lexer.skip_token();

                    ::std::unique_ptr<AST::ExprStmtWithoutBlock> stmt(
                      new AST::ExprStmtWithoutBlock(::std::move(expr), stmt_or_expr_loc));
                    return ExprOrStmt(::std::move(stmt));
                }

                return ExprOrStmt(::std::move(expr));
            }
        }
    }

    // Parses a struct expression field.
    ::std::unique_ptr<AST::StructExprField> Parser::parse_struct_expr_field() {
        // DEBUG:
        fprintf(stderr, "beginning struct/enum expr field parsing \n");

        const_TokenPtr t = lexer.peek_token();
        switch (t->get_id()) {
            case IDENTIFIER:
                if (lexer.peek_token(1)->get_id() == COLON) {
                    // struct expr field with identifier and expr
                    Identifier ident = t->get_str();
                    lexer.skip_token(1);

                    // parse expression (required)
                    ::std::unique_ptr<AST::Expr> expr = parse_expr();
                    if (expr == NULL) {
                        error_at(t->get_locus(),
                          "failed to parse struct expression field with identifier and expression");
                        return NULL;
                    }

                    // DEBUG:
                    fprintf(stderr, "struct/enum expr field parsing field identifier value done \n");

                    return ::std::unique_ptr<AST::StructExprFieldIdentifierValue>(
                      new AST::StructExprFieldIdentifierValue(::std::move(ident), ::std::move(expr)));
                } else {
                    // struct expr field with identifier only
                    Identifier ident = t->get_str();
                    lexer.skip_token();

                    // DEBUG:
                    fprintf(stderr, "struct/enum expr field parsing field identifier done \n");

                    return ::std::unique_ptr<AST::StructExprFieldIdentifier>(
                      new AST::StructExprFieldIdentifier(::std::move(ident)));
                }
            case INT_LITERAL: {
                // parse tuple index field
                int index = atoi(t->get_str().c_str());
                lexer.skip_token();

                if (!skip_token(COLON)) {
                    // skip somewhere?
                    return NULL;
                }

                // parse field expression (required)
                ::std::unique_ptr<AST::Expr> expr = parse_expr();
                if (expr == NULL) {
                    error_at(t->get_locus(),
                      "failed to parse expr in struct (or enum) expr field with tuple index");
                    return NULL;
                }

                // DEBUG:
                fprintf(stderr, "struct/enum expr field parsing field index value done \n");

                return ::std::unique_ptr<AST::StructExprFieldIndexValue>(
                  new AST::StructExprFieldIndexValue(index, ::std::move(expr)));
            }
            case DOT_DOT:
                // this is a struct base and can't be parsed here, so just return nothing without
                // erroring

                // DEBUG:
                fprintf(stderr, "struct/enum expr field parsing failed - '..' \n");

                return NULL;
            default:
                // DEBUG:
                fprintf(stderr, "struct/enum expr field parsing failed - unrecognised char \n");

                error_at(t->get_locus(),
                  "unrecognised token '%s' as first token of struct expr field - expected identifier "
                  "or int literal",
                  t->get_token_description());
                return NULL;
        }
    }

    // Parses a macro invocation or macro invocation semi.
    ExprOrStmt Parser::parse_macro_invocation_maybe_semi(::std::vector<AST::Attribute> outer_attrs) {
        location_t macro_locus = lexer.peek_token()->get_locus();
        AST::SimplePath macro_path = parse_simple_path();
        if (macro_path.is_empty()) {
            error_at(lexer.peek_token()->get_locus(),
              "failed to parse simple path in macro invocation or semi");
            return ExprOrStmt::create_error();
        }

        if (!skip_token(EXCLAM)) {
            return ExprOrStmt::create_error();
        }

        const_TokenPtr t3 = lexer.peek_token();
        location_t tok_tree_loc = t3->get_locus();

        AST::DelimType type = AST::PARENS;
        switch (t3->get_id()) {
            case LEFT_PAREN:
                type = AST::PARENS;
                break;
            case LEFT_SQUARE:
                type = AST::SQUARE;
                break;
            case LEFT_CURLY:
                type = AST::CURLY;
                break;
            default:
                error_at(t3->get_locus(),
                  "unrecognised token '%s' in macro invocation - (opening) "
                  "delimiter expected",
                  t3->get_token_description());
                return ExprOrStmt::create_error();
        }
        lexer.skip_token();

        // parse actual token trees
        ::std::vector< ::std::unique_ptr<AST::TokenTree> > token_trees;

        t3 = lexer.peek_token();
        // parse token trees until the initial delimiter token is found again
        while (!token_id_matches_delims(t3->get_id(), type)) {
            ::std::unique_ptr<AST::TokenTree> tree = parse_token_tree();

            if (tree == NULL) {
                error_at(t3->get_locus(),
                  "failed to parse token tree for macro invocation (or semi) - found '%s'",
                  t3->get_token_description());
                return ExprOrStmt::create_error();
            }

            token_trees.push_back(::std::move(tree));

            t3 = lexer.peek_token();
        }

        // parse end delimiters
        t3 = lexer.peek_token();
        if (token_id_matches_delims(t3->get_id(), type)) {
            // tokens match opening delimiter, so skip.
            lexer.skip_token();

            /* with curly bracketed macros, assume it is a macro invocation unless
             * a semicolon is explicitly put at the end. this is not necessarily
             * true (i.e. context-dependence) and so may have to be fixed up via
             * HACKs in semantic
             * analysis (by checking whether it is the last elem in the vector).
             */

            if (lexer.peek_token()->get_id() == SEMICOLON) {
                lexer.skip_token();

                ::std::unique_ptr<AST::MacroInvocationSemi> stmt(
                  new AST::MacroInvocationSemi(::std::move(macro_path), type,
                    ::std::move(token_trees), ::std::move(outer_attrs), macro_locus));
                return ExprOrStmt(::std::move(stmt));
            }

            // otherwise, create macro invocation
            AST::DelimTokenTree delim_tok_tree(type, ::std::move(token_trees), tok_tree_loc);

            ::std::unique_ptr<AST::MacroInvocation> expr(
              new AST::MacroInvocation(::std::move(macro_path), ::std::move(delim_tok_tree),
                ::std::move(outer_attrs), macro_locus));
            return ExprOrStmt(::std::move(expr));
        } else {
            const_TokenPtr t = lexer.peek_token();
            // tokens don't match opening delimiters, so produce error
            error_at(t->get_locus(),
              "unexpected token '%s' - expecting closing delimiter '%s' (for a "
              "macro invocation)",
              t->get_token_description(),
              (type == AST::PARENS ? ")" : (type == AST::SQUARE ? "]" : "}")));
            return ExprOrStmt::create_error();
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

#if 0
    // Parses a variable declaration statement.
    Tree Parser::parse_variable_declaration() {
        // skip initial var keyword - TODO: fix
        if (!skip_token(/*VAR*/ COLON)) {
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
#endif

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

#if 0
    // Parses type in variable declaration.
    Tree Parser::parse_type() {
        const_TokenPtr t = lexer.peek_token();

        Tree type;

        switch (t->get_id()) {
            // TODO: fix
            case /*INT*/ COLON:
                lexer.skip_token();
                type = integer_type_node;
                break;
            case /*FLOAT*/ ELLIPSIS:
                lexer.skip_token();
                type = float_type_node;
                break;
            case /*BOOL*/ BREAK:
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
            case /*RECORD*/ DOT:
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
#endif

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
        skip_token(LEFT_CURLY);

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
            skip_token(RIGHT_CURLY);
        } else if (tok->get_id() == RIGHT_CURLY) {
            // Consume 'end'
            skip_token(RIGHT_CURLY);
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

    // Skips all tokens until EOF or }. Don't use.
    void Parser::skip_after_end() {
        const_TokenPtr t = lexer.peek_token();

        while (t->get_id() != END_OF_FILE && t->get_id() != RIGHT_CURLY) {
            lexer.skip_token();
            t = lexer.peek_token();
        }

        if (t->get_id() == RIGHT_CURLY) {
            lexer.skip_token();
        }
    }

    /* A slightly more aware error-handler that skips all tokens until it reaches the end of the
     * block scope (i.e. when left curly brackets = right curly brackets). Note: assumes currently in
     * the middle of a block. Use skip_after_next_block to skip based on the assumption that the block
     * has not been entered yet. */
    void Parser::skip_after_end_block() {
        const_TokenPtr t = lexer.peek_token();
        int curly_count = 1;

        while (curly_count > 0 && t->get_id() != END_OF_FILE) {
            switch (t->get_id()) {
                case LEFT_CURLY:
                    curly_count++;
                    break;
                case RIGHT_CURLY:
                    curly_count--;
                    break;
                default:
                    break;
            }
            lexer.skip_token();
            t = lexer.peek_token();
        }
    }

    /* Skips tokens until the end of the next block. i.e. assumes that the block has not been entered
     * yet. */
    void Parser::skip_after_next_block() {
        const_TokenPtr t = lexer.peek_token();

        // initial loop - skip until EOF if no left curlies encountered
        while (t->get_id() != END_OF_FILE && t->get_id() != LEFT_CURLY) {
            lexer.skip_token();

            t = lexer.peek_token();
        }

        // if next token is left, skip it and then skip after the block ends
        if (t->get_id() == LEFT_CURLY) {
            lexer.skip_token();

            skip_after_end_block();
        }
        // otherwise, do nothing as EOF
    }

    // Skips all tokens until ] (the end of an attribute) - does not skip the ] (as designed for
    // attribute body use)
    void Parser::skip_after_end_attribute() {
        const_TokenPtr t = lexer.peek_token();

        while (t->get_id() != RIGHT_SQUARE) {
            lexer.skip_token();
            t = lexer.peek_token();
        }

        // Don't skip the RIGHT_SQUARE token
        /*if (t->get_id() == RIGHT_SQUARE) {
            lexer.skip_token();
        }*/
    }

    /* Pratt parser impl of parse_expr. FIXME: this is only provisional and probably will be changed.
     * FIXME: this may only parse expressions without blocks as they are the only expressions to have
     * precedence? */
    ::std::unique_ptr<AST::Expr> Parser::parse_expr(int right_binding_power,
      ::std::vector<AST::Attribute> outer_attrs, ParseRestrictions restrictions) {
        const_TokenPtr current_token = lexer.peek_token();
        lexer.skip_token();

        // parse null denotation (unary part of expression)
        ::std::unique_ptr<AST::Expr> expr
          = null_denotation_NEW(current_token, ::std::move(outer_attrs), restrictions);

        // DEBUG
        fprintf(stderr, "finished parsing null denotation\n");

        if (expr == NULL) {
            // DEBUG
            fprintf(stderr, "null denotation is null; returning null for parse_expr\n");
            return NULL;
        }

        // DEBUG
        fprintf(stderr, "null denotation is not null - going on to left denotation\n");

        // DEBUG
        fprintf(stderr, "initial rbp: '%i', initial lbp: '%i' (for '%s')\n", right_binding_power,
          left_binding_power(lexer.peek_token()), lexer.peek_token()->get_token_description());

        // stop parsing if find lower priority token - parse higher priority first
        while (right_binding_power < left_binding_power(lexer.peek_token())) {
            current_token = lexer.peek_token();
            lexer.skip_token();

            expr = left_denotation(
              current_token, ::std::move(expr), ::std::vector<AST::Attribute>(), restrictions);

            // DEBUG
            fprintf(stderr, "successfully got left_denotation in parse_expr \n");

            if (expr == NULL) {

                // DEBUG
                fprintf(stderr, "left denotation is null; returning null for parse_expr\n");

                return NULL;
            }

            // DEBUG
            fprintf(stderr, "left denotation is not null - going to next iteration \n");
        }

        // DEBUG
        fprintf(stderr, "successfully parsed expr in parse_expr - returning \n");

        return expr;
    }

    /* Parse expression with lowest left binding power. FIXME: this may only apply to expressions
     * without blocks as they are the only ones to have precedence? */
    ::std::unique_ptr<AST::Expr> Parser::parse_expr(
      ::std::vector<AST::Attribute> outer_attrs, ParseRestrictions restrictions) {
        // HACK: only call parse_expr(LBP_LOWEST) after ensuring it is not an expression with block?
        return parse_expr(LBP_LOWEST, ::std::move(outer_attrs), restrictions);
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

    /* Determines action to take when finding token at beginning of expression.
     * FIXME: this may only apply to precedence-capable expressions (which are all expressions without
     * blocks), so make return type ExprWithoutBlock? It would simplify stuff. */
    ::std::unique_ptr<AST::Expr> Parser::null_denotation_NEW(
      const_TokenPtr tok, ::std::vector<AST::Attribute> outer_attrs, ParseRestrictions restrictions) {
        // note: tok is previous character in input stream, not current one, as parse_expr
        // skips it before passing it in

        /* as a Pratt parser (which works by decomposing expressions into a null denotation and then a
         * left denotation), null denotations handle primaries and unary operands (but only prefix
         * unary operands) */

        switch (tok->get_id()) {
            /*case IDENTIFIER: {
                // when encountering identifier, lookup in scope
                SymbolPtr s = scope.lookup(tok->get_str());
                if (s == NULL) {
                    error_at(tok->get_locus(), "variable '%s' not declared in the current scope",
                      tok->get_str().c_str());

                    return Tree::error();
                }
                // expression is just its VAR_DECL that was stored in the Symbol at declaration
                return Tree(s->get_tree_decl(), tok->get_locus());
            }*/
            // symbol table must be created in semantic analysis pass, so can't use this
            case IDENTIFIER: {
                // DEBUG
                fprintf(stderr, "beginning null denotation identifier handling\n");

                // best option: parse as path, then extract identifier, macro, struct/enum, or just
                // path info from it
                AST::PathInExpression path = parse_path_in_expression_pratt(tok);

                // DEBUG:
                fprintf(
                  stderr, "finished null denotation identifier path parsing - next is branching \n");

                // branch on next token
                const_TokenPtr t = lexer.peek_token();
                switch (t->get_id()) {
                    case EXCLAM:
                        // macro
                        return parse_macro_invocation_partial(
                          ::std::move(path), ::std::move(outer_attrs));
                    case LEFT_CURLY: {
                        bool not_a_block
                          = lexer.peek_token(1)->get_id() == IDENTIFIER
                            && (lexer.peek_token(2)->get_id() == COMMA
                                 || (lexer.peek_token(2)->get_id() == COLON
                                      && (lexer.peek_token(4)->get_id() == COMMA
                                           || !can_tok_start_type(lexer.peek_token(3)->get_id()))));

                        /* definitely not a block:
                         *  path '{' ident ','
                         *  path '{' ident ':' [anything] ','
                         *  path '{' ident ':' [not a type]
                         * otherwise, assume block expr and thus path */
                        // DEBUG
                        fprintf(stderr, "values of lookahead: '%s' '%s' '%s' '%s' \n",
                          lexer.peek_token(1)->get_token_description(),
                          lexer.peek_token(2)->get_token_description(),
                          lexer.peek_token(3)->get_token_description(),
                          lexer.peek_token(4)->get_token_description());

                        fprintf(stderr, "can be struct expr: '%s', not a block: '%s'\n",
                          restrictions.can_be_struct_expr ? "true" : "false",
                          not_a_block ? "true" : "false");

                        // struct/enum expr struct
                        if (!restrictions.can_be_struct_expr && !not_a_block) {
                            // assume path is returned if not single segment
                            if (path.is_single_segment()) {
                                // have to return an identifier expression or something, idk
                                // HACK: may have to become permanent, but this is my current
                                // identifier expression
                                return ::std::unique_ptr<AST::IdentifierExpr>(
                                  new AST::IdentifierExpr(tok->get_str(), tok->get_locus()));
                            }
                            // HACK: add outer attrs to path
                            path.replace_outer_attrs(::std::move(outer_attrs));
                            return ::std::unique_ptr<AST::PathInExpression>(
                              new AST::PathInExpression(::std::move(path)));
                        }
                        return parse_struct_expr_struct_partial(
                          ::std::move(path), ::std::move(outer_attrs));
                    }
                    case LEFT_PAREN:
                        // struct/enum expr tuple
                        if (!restrictions.can_be_struct_expr) {
                            // assume path is returned if not single segment
                            if (path.is_single_segment()) {
                                // have to return an identifier expression or something, idk
                                // HACK: may have to become permanent, but this is my current
                                // identifier expression
                                return ::std::unique_ptr<AST::IdentifierExpr>(
                                  new AST::IdentifierExpr(tok->get_str(), tok->get_locus()));
                            }
                            // HACK: add outer attrs to path
                            path.replace_outer_attrs(::std::move(outer_attrs));
                            return ::std::unique_ptr<AST::PathInExpression>(
                              new AST::PathInExpression(::std::move(path)));
                        }
                        return parse_struct_expr_tuple_partial(
                          ::std::move(path), ::std::move(outer_attrs));
                    default:
                        // assume path is returned if not single segment
                        if (path.is_single_segment()) {
                            // have to return an identifier expression or something, idk
                            // HACK: may have to become permanent, but this is my current identifier
                            // expression
                            return ::std::unique_ptr<AST::IdentifierExpr>(
                              new AST::IdentifierExpr(tok->get_str(), tok->get_locus()));
                        }
                        // HACK: add outer attrs to path
                        path.replace_outer_attrs(::std::move(outer_attrs));
                        return ::std::unique_ptr<AST::PathInExpression>(
                          new AST::PathInExpression(::std::move(path)));
                }
                gcc_unreachable();
            }
            // FIXME: delegate to parse_literal_expr instead? would have to rejig tokens and whatever.
            // FIXME: could also be path expression (and hence macro expression, struct/enum expr)
            case LEFT_ANGLE: {
                // qualified path
                // HACK: add outer attrs to path
                AST::QualifiedPathInExpression path = parse_qualified_path_in_expression(true);
                path.replace_outer_attrs(::std::move(outer_attrs));
                return ::std::unique_ptr<AST::QualifiedPathInExpression>(
                  new AST::QualifiedPathInExpression(::std::move(path)));
            }
            case INT_LITERAL:
                // we should check the range, but ignore for now
                // encode as int?
                return ::std::unique_ptr<AST::LiteralExpr>(
                  new AST::LiteralExpr(tok->get_str(), AST::Literal::INT, tok->get_locus()));
            case FLOAT_LITERAL:
                // encode as float?
                return ::std::unique_ptr<AST::LiteralExpr>(
                  new AST::LiteralExpr(tok->get_str(), AST::Literal::FLOAT, tok->get_locus()));
            case STRING_LITERAL:
                return ::std::unique_ptr<AST::LiteralExpr>(
                  new AST::LiteralExpr(tok->get_str(), AST::Literal::STRING, tok->get_locus()));
            case TRUE_LITERAL:
                return ::std::unique_ptr<AST::LiteralExpr>(
                  new AST::LiteralExpr("true", AST::Literal::BOOL, tok->get_locus()));
            case FALSE_LITERAL:
                return ::std::unique_ptr<AST::LiteralExpr>(
                  new AST::LiteralExpr("false", AST::Literal::BOOL, tok->get_locus()));
            case LEFT_PAREN: { // have to parse whole expression if inside brackets
                /* recursively invoke parse_expression with lowest priority possible as it it were
                 * a top-level expression. */
                /*AST::Expr* expr = parse_expr();
                tok = lexer.peek_token();

                // end of expression must be a close-bracket
                if (tok->get_id() != RIGHT_PAREN)
                    error_at(
                      tok->get_locus(), "expecting ')' but %s found\n", tok->get_token_description());
                else
                    lexer.skip_token();

                return expr;
                // FIXME: this assumes grouped expression - could be tuple expression if commas
                inside*/

                return parse_grouped_or_tuple_expr(::std::move(outer_attrs), true);
            }
            /*case PLUS: { // unary plus operator
                // invoke parse_expr recursively with appropriate priority, etc. for below
                AST::Expr* expr = parse_expr(LBP_UNARY_PLUS);

                if (expr == NULL)
                    return NULL;
                // can only apply to integer and float expressions
                if (expr->get_type() != integer_type_node || expr->get_type() != float_type_node) {
                    error_at(tok->get_locus(),
                      "operand of unary plus must be int or float but it is %s",
                      print_type(expr->get_type()));
                    return NULL;
                }

                return Tree(expr, tok->get_locus());
            }*/
            // Rust has no unary plus operator
            case MINUS: { // unary minus
                ParseRestrictions entered_from_unary;
                entered_from_unary.entered_from_unary = true;
                ::std::unique_ptr<AST::Expr> expr
                  = parse_expr(LBP_UNARY_MINUS, ::std::vector<AST::Attribute>(), entered_from_unary);

                if (expr == NULL)
                    return NULL;
                // can only apply to integer and float expressions
                /*if (expr.get_type() != integer_type_node || expr.get_type() != float_type_node) {
                    error_at(tok->get_locus(),
                      "operand of unary minus must be int or float but it is %s",
                      print_type(expr.get_type()));
                    return Tree::error();
                }*/
                /* FIXME: when implemented the "get type" method on expr, ensure it is int or float
                 * type (except unsigned int). Actually, this would probably have to be done in
                 * semantic analysis (as type checking). */

                /* FIXME: allow outer attributes on these expressions by having an outer attrs
                 * parameter in function*/
                return ::std::unique_ptr<AST::NegationExpr>(new AST::NegationExpr(::std::move(expr),
                  AST::NegationExpr::NEGATE, ::std::move(outer_attrs), tok->get_locus()));
            }
            case EXCLAM: { // logical or bitwise not
                ParseRestrictions entered_from_unary;
                entered_from_unary.entered_from_unary = true;
                ::std::unique_ptr<AST::Expr> expr
                  = parse_expr(LBP_UNARY_EXCLAM, ::std::vector<AST::Attribute>(), entered_from_unary);

                if (expr == NULL)
                    return NULL;
                // can only apply to boolean expressions
                /*if (expr.get_type() != boolean_type_node) {
                    error_at(tok->get_locus(),
                      "operand of logical not must be a boolean but it is %s",
                      print_type(expr.get_type()));
                    return Tree::error();
                }*/
                // FIXME: type checking for boolean or integer expressions in semantic analysis

                // FIXME: allow outer attributes on these expressions
                return ::std::unique_ptr<AST::NegationExpr>(new AST::NegationExpr(::std::move(expr),
                  AST::NegationExpr::NOT, ::std::move(outer_attrs), tok->get_locus()));
            }
            case ASTERISK: {
                // pointer dereference only - HACK: as struct expressions should always be value
                // expressions, cannot be dereferenced
                ParseRestrictions entered_from_unary;
                entered_from_unary.entered_from_unary = true;
                entered_from_unary.can_be_struct_expr = false;
                ::std::unique_ptr<AST::Expr> expr = parse_expr(
                  LBP_UNARY_ASTERISK, ::std::vector<AST::Attribute>(), entered_from_unary);
                // FIXME: allow outer attributes on expression
                return ::std::unique_ptr<AST::DereferenceExpr>(new AST::DereferenceExpr(
                  ::std::move(expr), ::std::move(outer_attrs), tok->get_locus()));
            }
            case AMP: {
                // (single) "borrow" expression - shared (mutable) or immutable
                ::std::unique_ptr<AST::Expr> expr = NULL;
                bool is_mut_borrow = false;

                // HACK: as struct expressions should always be value expressions, cannot be
                // referenced
                ParseRestrictions entered_from_unary;
                entered_from_unary.entered_from_unary = true;
                entered_from_unary.can_be_struct_expr = false;

                if (lexer.peek_token()->get_id() == MUT) {
                    lexer.skip_token();
                    expr = parse_expr(
                      LBP_UNARY_AMP_MUT, ::std::vector<AST::Attribute>(), entered_from_unary);
                    is_mut_borrow = true;
                } else {
                    expr = parse_expr(
                      LBP_UNARY_AMP, ::std::vector<AST::Attribute>(), entered_from_unary);
                }

                // FIXME: allow outer attributes on expression
                return ::std::unique_ptr<AST::BorrowExpr>(new AST::BorrowExpr(::std::move(expr),
                  is_mut_borrow, false, ::std::move(outer_attrs), tok->get_locus()));
            }
            case LOGICAL_AND: {
                // (double) "borrow" expression - shared (mutable) or immutable
                ::std::unique_ptr<AST::Expr> expr = NULL;
                bool is_mut_borrow = false;

                ParseRestrictions entered_from_unary;
                entered_from_unary.entered_from_unary = true;

                if (lexer.peek_token()->get_id() == MUT) {
                    lexer.skip_token();
                    expr = parse_expr(
                      LBP_UNARY_AMP_MUT, ::std::vector<AST::Attribute>(), entered_from_unary);
                    is_mut_borrow = true;
                } else {
                    expr = parse_expr(
                      LBP_UNARY_AMP, ::std::vector<AST::Attribute>(), entered_from_unary);
                }

                // FIXME: allow outer attributes on expression
                return ::std::unique_ptr<AST::BorrowExpr>(new AST::BorrowExpr(::std::move(expr),
                  is_mut_borrow, true, ::std::move(outer_attrs), tok->get_locus()));
            }
            case SCOPE_RESOLUTION: {
                // TODO: fix: this is for global paths, i.e. ::std::string::whatever
                error_at(tok->get_locus(), "found null denotation scope resolution operator, and "
                                           "haven't written handling for it.");
                return NULL;
            }
            case SELF:
            case SELF_ALIAS:
            case DOLLAR_SIGN:
            case CRATE:
            case SUPER: {
                // DEBUG
                fprintf(
                  stderr, "beginning null denotation self/self-alias/dollar/crate/super handling\n");

                // best option: parse as path, then extract identifier, macro, struct/enum, or just
                // path info from it
                AST::PathInExpression path = parse_path_in_expression_pratt(tok);

                // DEBUG
                fprintf(stderr,
                  "just finished parsing path (going to disambiguate) - peeked token is '%s'\n",
                  lexer.peek_token()->get_token_description());

                // HACK: always make "self" by itself a path (regardless of next tokens)
                if (tok->get_id() == SELF && path.is_single_segment()) {
                    // HACK: add outer attrs to path
                    path.replace_outer_attrs(::std::move(outer_attrs));
                    return ::std::unique_ptr<AST::PathInExpression>(
                      new AST::PathInExpression(::std::move(path)));
                }

                // branch on next token
                const_TokenPtr t = lexer.peek_token();
                switch (t->get_id()) {
                    case EXCLAM:
                        // macro
                        return parse_macro_invocation_partial(
                          ::std::move(path), ::std::move(outer_attrs));
                    case LEFT_CURLY: {
                        // struct/enum expr struct
                        fprintf(stderr, "can_be_struct_expr: %s\n",
                          restrictions.can_be_struct_expr ? "true" : "false");

                        bool not_a_block
                          = lexer.peek_token(1)->get_id() == IDENTIFIER
                            && (lexer.peek_token(2)->get_id() == COMMA
                                 || (lexer.peek_token(2)->get_id() == COLON
                                      && (lexer.peek_token(4)->get_id() == COMMA
                                           || !can_tok_start_type(lexer.peek_token(3)->get_id()))));

                        if (!restrictions.can_be_struct_expr && !not_a_block) {
                            // assume path is returned
                            // HACK: add outer attributes to path
                            path.replace_outer_attrs(::std::move(outer_attrs));
                            return ::std::unique_ptr<AST::PathInExpression>(
                              new AST::PathInExpression(::std::move(path)));
                        }
                        return parse_struct_expr_struct_partial(
                          ::std::move(path), ::std::move(outer_attrs));
                    }
                    case LEFT_PAREN:
                        // struct/enum expr tuple
                        if (!restrictions.can_be_struct_expr) {
                            // assume path is returned
                            // HACK: add outer attributes to path
                            path.replace_outer_attrs(::std::move(outer_attrs));
                            return ::std::unique_ptr<AST::PathInExpression>(
                              new AST::PathInExpression(::std::move(path)));
                        }
                        return parse_struct_expr_tuple_partial(
                          ::std::move(path), ::std::move(outer_attrs));
                    default:
                        // assume path is returned
                        // HACK: add outer attributes to path
                        path.replace_outer_attrs(::std::move(outer_attrs));
                        return ::std::unique_ptr<AST::PathInExpression>(
                          new AST::PathInExpression(::std::move(path)));
                }
                gcc_unreachable();
            }
            case OR:
            case PIPE:
            case MOVE:
                // closure expression
                return parse_closure_expr_pratt(tok, ::std::move(outer_attrs));
            case DOT_DOT:
                // either "range to" or "range full" expressions
                return parse_nud_range_exclusive_expr(tok, ::std::move(outer_attrs));
            case DOT_DOT_EQ:
                // range to inclusive expr
                return parse_range_to_inclusive_expr(tok, ::std::move(outer_attrs));
            case RETURN_TOK:
                // FIXME: is this really a null denotation expression?
                return parse_return_expr(::std::move(outer_attrs), true);
            case BREAK:
                // FIXME: is this really a null denotation expression?
                return parse_break_expr(::std::move(outer_attrs), true);
            case CONTINUE:
                return parse_continue_expr(::std::move(outer_attrs), true);
            case LEFT_CURLY:
                // ok - this is an expression with block for once.
                return parse_block_expr(::std::move(outer_attrs), true);
            case MATCH_TOK:
                // also an expression with block
                return parse_match_expr(::std::move(outer_attrs), true);
            case LEFT_SQUARE:
                // array definition expr (not indexing)
                return parse_array_expr(::std::move(outer_attrs), true);
            default:
                error_at(tok->get_locus(), "found unexpected token '%s' in null denotation",
                  tok->get_token_description());
                return NULL;
        }
    }

    /* Called for each token that can appear in infix (between) position. Can be operators or other
     * punctuation.
     * Returns a function pointer to member function that implements the left denotation for the token
     * given. */
    ::std::unique_ptr<AST::Expr> Parser::left_denotation(const_TokenPtr tok,
      ::std::unique_ptr<AST::Expr> left, ::std::vector<AST::Attribute> outer_attrs,
      ParseRestrictions restrictions) {
        // Token passed in has already been skipped, so peek gives "next" token
        /*BinaryHandler binary_handler = get_binary_handler(tok->get_id());
        if (binary_handler == NULL) {
            unexpected_token(tok);
            return NULL;
        }

        return (this->*binary_handler)(tok, left);*/
        // can't do with binary handler because same token used for several operators

        switch (tok->get_id()) {
            // FIXME: allow for outer attributes to be applied
            case QUESTION_MARK: {
                location_t left_locus = left->get_locus_slow();
                // error propagation expression - unary postfix
                return ::std::unique_ptr<AST::ErrorPropagationExpr>(new AST::ErrorPropagationExpr(
                  ::std::move(left), ::std::move(outer_attrs), left_locus));
            }
            case PLUS:
                // sum expression - binary infix
                return parse_binary_plus_expr(
                  tok, ::std::move(left), ::std::move(outer_attrs), restrictions);
            case MINUS:
                // difference expression - binary infix
                return parse_binary_minus_expr(
                  tok, ::std::move(left), ::std::move(outer_attrs), restrictions);
            case ASTERISK:
                // product expression - binary infix
                return parse_binary_mult_expr(
                  tok, ::std::move(left), ::std::move(outer_attrs), restrictions);
            case DIV:
                // quotient expression - binary infix
                return parse_binary_div_expr(
                  tok, ::std::move(left), ::std::move(outer_attrs), restrictions);
            case PERCENT:
                // modulo expression - binary infix
                return parse_binary_mod_expr(
                  tok, ::std::move(left), ::std::move(outer_attrs), restrictions);
            case AMP:
                // logical or bitwise and expression - binary infix
                return parse_bitwise_and_expr(
                  tok, ::std::move(left), ::std::move(outer_attrs), restrictions);
            case PIPE:
                // logical or bitwise or expression - binary infix
                return parse_bitwise_or_expr(
                  tok, ::std::move(left), ::std::move(outer_attrs), restrictions);
            case CARET:
                // logical or bitwise xor expression - binary infix
                return parse_bitwise_xor_expr(
                  tok, ::std::move(left), ::std::move(outer_attrs), restrictions);
            case LEFT_SHIFT:
                // left shift expression - binary infix
                return parse_left_shift_expr(
                  tok, ::std::move(left), ::std::move(outer_attrs), restrictions);
            case RIGHT_SHIFT:
                // right shift expression - binary infix
                return parse_right_shift_expr(
                  tok, ::std::move(left), ::std::move(outer_attrs), restrictions);
            case EQUAL_EQUAL:
                // equal to expression - binary infix (no associativity)
                return parse_binary_equal_expr(
                  tok, ::std::move(left), ::std::move(outer_attrs), restrictions);
            case NOT_EQUAL:
                // not equal to expression - binary infix (no associativity)
                return parse_binary_not_equal_expr(
                  tok, ::std::move(left), ::std::move(outer_attrs), restrictions);
            case RIGHT_ANGLE:
                // greater than expression - binary infix (no associativity)
                return parse_binary_greater_than_expr(
                  tok, ::std::move(left), ::std::move(outer_attrs), restrictions);
            case LEFT_ANGLE:
                // less than expression - binary infix (no associativity)
                return parse_binary_less_than_expr(
                  tok, ::std::move(left), ::std::move(outer_attrs), restrictions);
            case GREATER_OR_EQUAL:
                // greater than or equal to expression - binary infix (no associativity)
                return parse_binary_greater_equal_expr(
                  tok, ::std::move(left), ::std::move(outer_attrs), restrictions);
            case LESS_OR_EQUAL:
                // less than or equal to expression - binary infix (no associativity)
                return parse_binary_less_equal_expr(
                  tok, ::std::move(left), ::std::move(outer_attrs), restrictions);
            case OR:
                // lazy logical or expression - binary infix
                return parse_lazy_or_expr(
                  tok, ::std::move(left), ::std::move(outer_attrs), restrictions);
            case LOGICAL_AND:
                // lazy logical and expression - binary infix
                return parse_lazy_and_expr(
                  tok, ::std::move(left), ::std::move(outer_attrs), restrictions);
            case AS:
                // type cast expression - kind of binary infix (RHS is actually a TypeNoBounds)
                return parse_type_cast_expr(
                  tok, ::std::move(left), ::std::move(outer_attrs), restrictions);
            case EQUAL:
                // assignment expression - binary infix (note right-to-left associativity)
                return parse_assig_expr(
                  tok, ::std::move(left), ::std::move(outer_attrs), restrictions);
            case PLUS_EQ:
                // plus-assignment expression - binary infix (note right-to-left associativity)
                return parse_plus_assig_expr(
                  tok, ::std::move(left), ::std::move(outer_attrs), restrictions);
            case MINUS_EQ:
                // minus-assignment expression - binary infix (note right-to-left associativity)
                return parse_minus_assig_expr(
                  tok, ::std::move(left), ::std::move(outer_attrs), restrictions);
            case ASTERISK_EQ:
                // multiply-assignment expression - binary infix (note right-to-left associativity)
                return parse_mult_assig_expr(
                  tok, ::std::move(left), ::std::move(outer_attrs), restrictions);
            case DIV_EQ:
                // division-assignment expression - binary infix (note right-to-left associativity)
                return parse_div_assig_expr(
                  tok, ::std::move(left), ::std::move(outer_attrs), restrictions);
            case PERCENT_EQ:
                // modulo-assignment expression - binary infix (note right-to-left associativity)
                return parse_mod_assig_expr(
                  tok, ::std::move(left), ::std::move(outer_attrs), restrictions);
            case AMP_EQ:
                // bitwise and-assignment expression - binary infix (note right-to-left associativity)
                return parse_and_assig_expr(
                  tok, ::std::move(left), ::std::move(outer_attrs), restrictions);
            case PIPE_EQ:
                // bitwise or-assignment expression - binary infix (note right-to-left associativity)
                return parse_or_assig_expr(
                  tok, ::std::move(left), ::std::move(outer_attrs), restrictions);
            case CARET_EQ:
                // bitwise xor-assignment expression - binary infix (note right-to-left associativity)
                return parse_xor_assig_expr(
                  tok, ::std::move(left), ::std::move(outer_attrs), restrictions);
            case LEFT_SHIFT_EQ:
                // left shift-assignment expression - binary infix (note right-to-left associativity)
                return parse_left_shift_assig_expr(
                  tok, ::std::move(left), ::std::move(outer_attrs), restrictions);
            case RIGHT_SHIFT_EQ:
                // right shift-assignment expression - binary infix (note right-to-left associativity)
                return parse_right_shift_assig_expr(
                  tok, ::std::move(left), ::std::move(outer_attrs), restrictions);
            case DOT_DOT:
                // range exclusive expression - binary infix (no associativity)
                // either "range" or "range from"
                return parse_led_range_exclusive_expr(
                  tok, ::std::move(left), ::std::move(outer_attrs), restrictions);
            case DOT_DOT_EQ:
                // range inclusive expression - binary infix (no associativity)
                // unambiguously RangeInclusiveExpr
                return parse_range_inclusive_expr(
                  tok, ::std::move(left), ::std::move(outer_attrs), restrictions);
            case SCOPE_RESOLUTION:
                // path expression - binary infix? FIXME should this even be parsed here?
                error_at(tok->get_locus(), "found scope resolution operator in left denotation "
                                           "function - this should probably be handled elsewhere.");
                return NULL;
            case DOT: {
                // field expression or method call - relies on parentheses after next identifier
                // or await if token after is "await" (unary postfix)
                // or tuple index if token after is a decimal int literal

                const_TokenPtr next_tok = lexer.peek_token();
                if (next_tok->get_id() == IDENTIFIER && next_tok->get_str() == "await") {
                    // await expression
                    return parse_await_expr(tok, ::std::move(left), ::std::move(outer_attrs));
                } else if (next_tok->get_id() == INT_LITERAL) {
                    // tuple index expression - TODO check for decimal int literal
                    return parse_tuple_index_expr(
                      tok, ::std::move(left), ::std::move(outer_attrs), restrictions);
                } else if (next_tok->get_id() == IDENTIFIER
                           && lexer.peek_token(1)->get_id() != LEFT_PAREN
                           && lexer.peek_token(1)->get_id() != SCOPE_RESOLUTION) {
                    // field expression (or should be) - FIXME: scope resolution right after
                    // identifier should always be method, I'm pretty sure
                    return parse_field_access_expr(
                      tok, ::std::move(left), ::std::move(outer_attrs), restrictions);
                } else {
                    // method call (probably)
                    return parse_method_call_expr(
                      tok, ::std::move(left), ::std::move(outer_attrs), restrictions);
                }
            }
            case LEFT_PAREN:
                // function call - method call is based on dot notation first
                return parse_function_call_expr(
                  tok, ::std::move(left), ::std::move(outer_attrs), restrictions);
            case LEFT_SQUARE:
                // array or slice index expression (pseudo binary infix)
                return parse_index_expr(
                  tok, ::std::move(left), ::std::move(outer_attrs), restrictions);
            case FLOAT_LITERAL:
                // HACK: get around lexer mis-identifying '.0' or '.1' or whatever as a float literal
                return parse_tuple_index_expr_float(
                  tok, ::std::move(left), ::std::move(outer_attrs), restrictions);
            default:
                error_at(tok->get_locus(), "found unexpected token '%s' in left denotation",
                  tok->get_token_description());
                return NULL;
        }
    }

    // Parses a binary addition expression (with Pratt parsing).
    ::std::unique_ptr<AST::ArithmeticOrLogicalExpr> Parser::parse_binary_plus_expr(
      const_TokenPtr tok ATTRIBUTE_UNUSED, ::std::unique_ptr<AST::Expr> left,
      ::std::vector<AST::Attribute> outer_attrs ATTRIBUTE_UNUSED, ParseRestrictions restrictions) {
        // parse RHS (as tok has already been consumed in parse_expression)
        ::std::unique_ptr<AST::Expr> right
          = parse_expr(LBP_PLUS, ::std::vector<AST::Attribute>(), restrictions);
        if (right == NULL)
            return NULL;

        // TODO: check types. actually, do so during semantic analysis
        location_t locus = left->get_locus_slow();

        return ::std::unique_ptr<AST::ArithmeticOrLogicalExpr>(new AST::ArithmeticOrLogicalExpr(
          ::std::move(left), ::std::move(right), AST::ArithmeticOrLogicalExpr::ADD, locus));
    }

    // Parses a binary subtraction expression (with Pratt parsing).
    ::std::unique_ptr<AST::ArithmeticOrLogicalExpr> Parser::parse_binary_minus_expr(
      const_TokenPtr tok ATTRIBUTE_UNUSED, ::std::unique_ptr<AST::Expr> left,
      ::std::vector<AST::Attribute> outer_attrs ATTRIBUTE_UNUSED, ParseRestrictions restrictions) {
        // parse RHS (as tok has already been consumed in parse_expression)
        ::std::unique_ptr<AST::Expr> right
          = parse_expr(LBP_MINUS, ::std::vector<AST::Attribute>(), restrictions);
        if (right == NULL)
            return NULL;

        // TODO: check types. actually, do so during semantic analysis
        location_t locus = left->get_locus_slow();

        return ::std::unique_ptr<AST::ArithmeticOrLogicalExpr>(new AST::ArithmeticOrLogicalExpr(
          ::std::move(left), ::std::move(right), AST::ArithmeticOrLogicalExpr::SUBTRACT, locus));
    }

    // Parses a binary multiplication expression (with Pratt parsing).
    ::std::unique_ptr<AST::ArithmeticOrLogicalExpr> Parser::parse_binary_mult_expr(
      const_TokenPtr tok ATTRIBUTE_UNUSED, ::std::unique_ptr<AST::Expr> left,
      ::std::vector<AST::Attribute> outer_attrs ATTRIBUTE_UNUSED, ParseRestrictions restrictions) {
        // parse RHS (as tok has already been consumed in parse_expression)
        ::std::unique_ptr<AST::Expr> right
          = parse_expr(LBP_MUL, ::std::vector<AST::Attribute>(), restrictions);
        if (right == NULL)
            return NULL;

        // TODO: check types. actually, do so during semantic analysis
        location_t locus = left->get_locus_slow();

        return ::std::unique_ptr<AST::ArithmeticOrLogicalExpr>(new AST::ArithmeticOrLogicalExpr(
          ::std::move(left), ::std::move(right), AST::ArithmeticOrLogicalExpr::MULTIPLY, locus));
    }

    // Parses a binary division expression (with Pratt parsing).
    ::std::unique_ptr<AST::ArithmeticOrLogicalExpr> Parser::parse_binary_div_expr(
      const_TokenPtr tok ATTRIBUTE_UNUSED, ::std::unique_ptr<AST::Expr> left,
      ::std::vector<AST::Attribute> outer_attrs ATTRIBUTE_UNUSED, ParseRestrictions restrictions) {
        // parse RHS (as tok has already been consumed in parse_expression)
        ::std::unique_ptr<AST::Expr> right
          = parse_expr(LBP_DIV, ::std::vector<AST::Attribute>(), restrictions);
        if (right == NULL)
            return NULL;

        // TODO: check types. actually, do so during semantic analysis
        location_t locus = left->get_locus_slow();

        return ::std::unique_ptr<AST::ArithmeticOrLogicalExpr>(new AST::ArithmeticOrLogicalExpr(
          ::std::move(left), ::std::move(right), AST::ArithmeticOrLogicalExpr::DIVIDE, locus));
    }

    // Parses a binary modulo expression (with Pratt parsing).
    ::std::unique_ptr<AST::ArithmeticOrLogicalExpr> Parser::parse_binary_mod_expr(
      const_TokenPtr tok ATTRIBUTE_UNUSED, ::std::unique_ptr<AST::Expr> left,
      ::std::vector<AST::Attribute> outer_attrs ATTRIBUTE_UNUSED, ParseRestrictions restrictions) {
        // parse RHS (as tok has already been consumed in parse_expression)
        ::std::unique_ptr<AST::Expr> right
          = parse_expr(LBP_MOD, ::std::vector<AST::Attribute>(), restrictions);
        if (right == NULL)
            return NULL;

        // TODO: check types. actually, do so during semantic analysis
        location_t locus = left->get_locus_slow();

        return ::std::unique_ptr<AST::ArithmeticOrLogicalExpr>(new AST::ArithmeticOrLogicalExpr(
          ::std::move(left), ::std::move(right), AST::ArithmeticOrLogicalExpr::MODULUS, locus));
    }

    // Parses a binary bitwise (or eager logical) and expression (with Pratt parsing).
    ::std::unique_ptr<AST::ArithmeticOrLogicalExpr> Parser::parse_bitwise_and_expr(
      const_TokenPtr tok ATTRIBUTE_UNUSED, ::std::unique_ptr<AST::Expr> left,
      ::std::vector<AST::Attribute> outer_attrs ATTRIBUTE_UNUSED, ParseRestrictions restrictions) {
        // parse RHS (as tok has already been consumed in parse_expression)
        ::std::unique_ptr<AST::Expr> right
          = parse_expr(LBP_AMP, ::std::vector<AST::Attribute>(), restrictions);
        if (right == NULL)
            return NULL;

        // TODO: check types. actually, do so during semantic analysis
        location_t locus = left->get_locus_slow();

        return ::std::unique_ptr<AST::ArithmeticOrLogicalExpr>(new AST::ArithmeticOrLogicalExpr(
          ::std::move(left), ::std::move(right), AST::ArithmeticOrLogicalExpr::BITWISE_AND, locus));
    }

    // Parses a binary bitwise (or eager logical) or expression (with Pratt parsing).
    ::std::unique_ptr<AST::ArithmeticOrLogicalExpr> Parser::parse_bitwise_or_expr(
      const_TokenPtr tok ATTRIBUTE_UNUSED, ::std::unique_ptr<AST::Expr> left,
      ::std::vector<AST::Attribute> outer_attrs ATTRIBUTE_UNUSED, ParseRestrictions restrictions) {
        // parse RHS (as tok has already been consumed in parse_expression)
        ::std::unique_ptr<AST::Expr> right
          = parse_expr(LBP_PIPE, ::std::vector<AST::Attribute>(), restrictions);
        if (right == NULL)
            return NULL;

        // TODO: check types. actually, do so during semantic analysis
        location_t locus = left->get_locus_slow();

        return ::std::unique_ptr<AST::ArithmeticOrLogicalExpr>(new AST::ArithmeticOrLogicalExpr(
          ::std::move(left), ::std::move(right), AST::ArithmeticOrLogicalExpr::BITWISE_OR, locus));
    }

    // Parses a binary bitwise (or eager logical) xor expression (with Pratt parsing).
    ::std::unique_ptr<AST::ArithmeticOrLogicalExpr> Parser::parse_bitwise_xor_expr(
      const_TokenPtr tok ATTRIBUTE_UNUSED, ::std::unique_ptr<AST::Expr> left,
      ::std::vector<AST::Attribute> outer_attrs ATTRIBUTE_UNUSED, ParseRestrictions restrictions) {
        // parse RHS (as tok has already been consumed in parse_expression)
        ::std::unique_ptr<AST::Expr> right
          = parse_expr(LBP_CARET, ::std::vector<AST::Attribute>(), restrictions);
        if (right == NULL)
            return NULL;

        // TODO: check types. actually, do so during semantic analysis
        location_t locus = left->get_locus_slow();

        return ::std::unique_ptr<AST::ArithmeticOrLogicalExpr>(new AST::ArithmeticOrLogicalExpr(
          ::std::move(left), ::std::move(right), AST::ArithmeticOrLogicalExpr::BITWISE_XOR, locus));
    }

    // Parses a binary left shift expression (with Pratt parsing).
    ::std::unique_ptr<AST::ArithmeticOrLogicalExpr> Parser::parse_left_shift_expr(
      const_TokenPtr tok ATTRIBUTE_UNUSED, ::std::unique_ptr<AST::Expr> left,
      ::std::vector<AST::Attribute> outer_attrs ATTRIBUTE_UNUSED, ParseRestrictions restrictions) {
        // parse RHS (as tok has already been consumed in parse_expression)
        ::std::unique_ptr<AST::Expr> right
          = parse_expr(LBP_L_SHIFT, ::std::vector<AST::Attribute>(), restrictions);
        if (right == NULL)
            return NULL;

        // TODO: check types. actually, do so during semantic analysis
        location_t locus = left->get_locus_slow();

        return ::std::unique_ptr<AST::ArithmeticOrLogicalExpr>(new AST::ArithmeticOrLogicalExpr(
          ::std::move(left), ::std::move(right), AST::ArithmeticOrLogicalExpr::LEFT_SHIFT, locus));
    }

    // Parses a binary right shift expression (with Pratt parsing).
    ::std::unique_ptr<AST::ArithmeticOrLogicalExpr> Parser::parse_right_shift_expr(
      const_TokenPtr tok ATTRIBUTE_UNUSED, ::std::unique_ptr<AST::Expr> left,
      ::std::vector<AST::Attribute> outer_attrs ATTRIBUTE_UNUSED, ParseRestrictions restrictions) {
        // parse RHS (as tok has already been consumed in parse_expression)
        ::std::unique_ptr<AST::Expr> right
          = parse_expr(LBP_R_SHIFT, ::std::vector<AST::Attribute>(), restrictions);
        if (right == NULL)
            return NULL;

        // TODO: check types. actually, do so during semantic analysis
        location_t locus = left->get_locus_slow();

        return ::std::unique_ptr<AST::ArithmeticOrLogicalExpr>(new AST::ArithmeticOrLogicalExpr(
          ::std::move(left), ::std::move(right), AST::ArithmeticOrLogicalExpr::RIGHT_SHIFT, locus));
    }

    // Parses a binary equal to expression (with Pratt parsing).
    ::std::unique_ptr<AST::ComparisonExpr> Parser::parse_binary_equal_expr(
      const_TokenPtr tok ATTRIBUTE_UNUSED, ::std::unique_ptr<AST::Expr> left,
      ::std::vector<AST::Attribute> outer_attrs ATTRIBUTE_UNUSED, ParseRestrictions restrictions) {
        // parse RHS (as tok has already been consumed in parse_expression)
        ::std::unique_ptr<AST::Expr> right
          = parse_expr(LBP_EQUAL, ::std::vector<AST::Attribute>(), restrictions);
        if (right == NULL)
            return NULL;

        // TODO: check types. actually, do so during semantic analysis
        location_t locus = left->get_locus_slow();

        return ::std::unique_ptr<AST::ComparisonExpr>(new AST::ComparisonExpr(
          ::std::move(left), ::std::move(right), AST::ComparisonExpr::EQUAL, locus));
    }

    // Parses a binary not equal to expression (with Pratt parsing).
    ::std::unique_ptr<AST::ComparisonExpr> Parser::parse_binary_not_equal_expr(
      const_TokenPtr tok ATTRIBUTE_UNUSED, ::std::unique_ptr<AST::Expr> left,
      ::std::vector<AST::Attribute> outer_attrs ATTRIBUTE_UNUSED, ParseRestrictions restrictions) {
        // parse RHS (as tok has already been consumed in parse_expression)
        ::std::unique_ptr<AST::Expr> right
          = parse_expr(LBP_NOT_EQUAL, ::std::vector<AST::Attribute>(), restrictions);
        if (right == NULL)
            return NULL;

        // TODO: check types. actually, do so during semantic analysis
        location_t locus = left->get_locus_slow();

        return ::std::unique_ptr<AST::ComparisonExpr>(new AST::ComparisonExpr(
          ::std::move(left), ::std::move(right), AST::ComparisonExpr::NOT_EQUAL, locus));
    }

    // Parses a binary greater than expression (with Pratt parsing).
    ::std::unique_ptr<AST::ComparisonExpr> Parser::parse_binary_greater_than_expr(
      const_TokenPtr tok ATTRIBUTE_UNUSED, ::std::unique_ptr<AST::Expr> left,
      ::std::vector<AST::Attribute> outer_attrs ATTRIBUTE_UNUSED, ParseRestrictions restrictions) {
        // parse RHS (as tok has already been consumed in parse_expression)
        ::std::unique_ptr<AST::Expr> right
          = parse_expr(LBP_GREATER_THAN, ::std::vector<AST::Attribute>(), restrictions);
        if (right == NULL)
            return NULL;

        // TODO: check types. actually, do so during semantic analysis
        location_t locus = left->get_locus_slow();

        return ::std::unique_ptr<AST::ComparisonExpr>(new AST::ComparisonExpr(
          ::std::move(left), ::std::move(right), AST::ComparisonExpr::GREATER_THAN, locus));
    }

    // Parses a binary less than expression (with Pratt parsing).
    ::std::unique_ptr<AST::ComparisonExpr> Parser::parse_binary_less_than_expr(
      const_TokenPtr tok ATTRIBUTE_UNUSED, ::std::unique_ptr<AST::Expr> left,
      ::std::vector<AST::Attribute> outer_attrs ATTRIBUTE_UNUSED, ParseRestrictions restrictions) {
        // parse RHS (as tok has already been consumed in parse_expression)
        ::std::unique_ptr<AST::Expr> right
          = parse_expr(LBP_SMALLER_THAN, ::std::vector<AST::Attribute>(), restrictions);
        if (right == NULL)
            return NULL;

        // TODO: check types. actually, do so during semantic analysis
        location_t locus = left->get_locus_slow();

        return ::std::unique_ptr<AST::ComparisonExpr>(new AST::ComparisonExpr(
          ::std::move(left), ::std::move(right), AST::ComparisonExpr::LESS_THAN, locus));
    }

    // Parses a binary greater than or equal to expression (with Pratt parsing).
    ::std::unique_ptr<AST::ComparisonExpr> Parser::parse_binary_greater_equal_expr(
      const_TokenPtr tok ATTRIBUTE_UNUSED, ::std::unique_ptr<AST::Expr> left,
      ::std::vector<AST::Attribute> outer_attrs ATTRIBUTE_UNUSED, ParseRestrictions restrictions) {
        // parse RHS (as tok has already been consumed in parse_expression)
        ::std::unique_ptr<AST::Expr> right
          = parse_expr(LBP_GREATER_EQUAL, ::std::vector<AST::Attribute>(), restrictions);
        if (right == NULL)
            return NULL;

        // TODO: check types. actually, do so during semantic analysis
        location_t locus = left->get_locus_slow();

        return ::std::unique_ptr<AST::ComparisonExpr>(new AST::ComparisonExpr(
          ::std::move(left), ::std::move(right), AST::ComparisonExpr::GREATER_OR_EQUAL, locus));
    }

    // Parses a binary less than or equal to expression (with Pratt parsing).
    ::std::unique_ptr<AST::ComparisonExpr> Parser::parse_binary_less_equal_expr(
      const_TokenPtr tok ATTRIBUTE_UNUSED, ::std::unique_ptr<AST::Expr> left,
      ::std::vector<AST::Attribute> outer_attrs ATTRIBUTE_UNUSED, ParseRestrictions restrictions) {
        // parse RHS (as tok has already been consumed in parse_expression)
        ::std::unique_ptr<AST::Expr> right
          = parse_expr(LBP_SMALLER_EQUAL, ::std::vector<AST::Attribute>(), restrictions);
        if (right == NULL)
            return NULL;

        // TODO: check types. actually, do so during semantic analysis
        location_t locus = left->get_locus_slow();

        return ::std::unique_ptr<AST::ComparisonExpr>(new AST::ComparisonExpr(
          ::std::move(left), ::std::move(right), AST::ComparisonExpr::LESS_OR_EQUAL, locus));
    }

    // Parses a binary lazy boolean or expression (with Pratt parsing).
    ::std::unique_ptr<AST::LazyBooleanExpr> Parser::parse_lazy_or_expr(
      const_TokenPtr tok ATTRIBUTE_UNUSED, ::std::unique_ptr<AST::Expr> left,
      ::std::vector<AST::Attribute> outer_attrs ATTRIBUTE_UNUSED, ParseRestrictions restrictions) {
        // parse RHS (as tok has already been consumed in parse_expression)
        ::std::unique_ptr<AST::Expr> right
          = parse_expr(LBP_LOGICAL_OR, ::std::vector<AST::Attribute>(), restrictions);
        if (right == NULL)
            return NULL;

        // TODO: check types. actually, do so during semantic analysis
        location_t locus = left->get_locus_slow();

        return ::std::unique_ptr<AST::LazyBooleanExpr>(new AST::LazyBooleanExpr(
          ::std::move(left), ::std::move(right), AST::LazyBooleanExpr::LOGICAL_OR, locus));
    }

    // Parses a binary lazy boolean and expression (with Pratt parsing).
    ::std::unique_ptr<AST::LazyBooleanExpr> Parser::parse_lazy_and_expr(
      const_TokenPtr tok ATTRIBUTE_UNUSED, ::std::unique_ptr<AST::Expr> left,
      ::std::vector<AST::Attribute> outer_attrs ATTRIBUTE_UNUSED, ParseRestrictions restrictions) {
        // parse RHS (as tok has already been consumed in parse_expression)
        ::std::unique_ptr<AST::Expr> right
          = parse_expr(LBP_LOGICAL_AND, ::std::vector<AST::Attribute>(), restrictions);
        if (right == NULL)
            return NULL;

        // TODO: check types. actually, do so during semantic analysis
        location_t locus = left->get_locus_slow();

        return ::std::unique_ptr<AST::LazyBooleanExpr>(new AST::LazyBooleanExpr(
          ::std::move(left), ::std::move(right), AST::LazyBooleanExpr::LOGICAL_AND, locus));
    }

    // Parses a pseudo-binary infix type cast expression (with Pratt parsing).
    ::std::unique_ptr<AST::TypeCastExpr> Parser::parse_type_cast_expr(
      const_TokenPtr tok ATTRIBUTE_UNUSED, ::std::unique_ptr<AST::Expr> expr_to_cast,
      ::std::vector<AST::Attribute> outer_attrs ATTRIBUTE_UNUSED,
      ParseRestrictions restrictions ATTRIBUTE_UNUSED) {
        // parse RHS (as tok has already been consumed in parse_expression)
        ::std::unique_ptr<AST::TypeNoBounds> type = parse_type_no_bounds();
        if (type == NULL)
            return NULL;
        // FIXME: how do I get precedence put in here?

        // TODO: check types. actually, do so during semantic analysis
        location_t locus = expr_to_cast->get_locus_slow();

        return ::std::unique_ptr<AST::TypeCastExpr>(
          new AST::TypeCastExpr(::std::move(expr_to_cast), ::std::move(type), locus));
    }

    // Parses a binary assignment expression (with Pratt parsing).
    ::std::unique_ptr<AST::AssignmentExpr> Parser::parse_assig_expr(
      const_TokenPtr tok ATTRIBUTE_UNUSED, ::std::unique_ptr<AST::Expr> left,
      ::std::vector<AST::Attribute> outer_attrs ATTRIBUTE_UNUSED, ParseRestrictions restrictions) {
        // parse RHS (as tok has already been consumed in parse_expression)
        ::std::unique_ptr<AST::Expr> right
          = parse_expr(LBP_ASSIG - 1, ::std::vector<AST::Attribute>(), restrictions);
        if (right == NULL)
            return NULL;
        // FIXME: ensure right-associativity for this - 'LBP - 1' may do this?

        // TODO: check types. actually, do so during semantic analysis
        location_t locus = left->get_locus_slow();

        return ::std::unique_ptr<AST::AssignmentExpr>(
          new AST::AssignmentExpr(::std::move(left), ::std::move(right), locus));
    }

    // Parses a binary add-assignment expression (with Pratt parsing).
    ::std::unique_ptr<AST::CompoundAssignmentExpr> Parser::parse_plus_assig_expr(
      const_TokenPtr tok ATTRIBUTE_UNUSED, ::std::unique_ptr<AST::Expr> left,
      ::std::vector<AST::Attribute> outer_attrs ATTRIBUTE_UNUSED, ParseRestrictions restrictions) {
        // parse RHS (as tok has already been consumed in parse_expression)
        ::std::unique_ptr<AST::Expr> right
          = parse_expr(LBP_PLUS_ASSIG - 1, ::std::vector<AST::Attribute>(), restrictions);
        if (right == NULL)
            return NULL;
        // FIXME: ensure right-associativity for this - 'LBP - 1' may do this?

        // TODO: check types. actually, do so during semantic analysis
        location_t locus = left->get_locus_slow();

        return ::std::unique_ptr<AST::CompoundAssignmentExpr>(new AST::CompoundAssignmentExpr(
          ::std::move(left), ::std::move(right), AST::CompoundAssignmentExpr::ADD, locus));
    }

    // Parses a binary minus-assignment expression (with Pratt parsing).
    ::std::unique_ptr<AST::CompoundAssignmentExpr> Parser::parse_minus_assig_expr(
      const_TokenPtr tok ATTRIBUTE_UNUSED, ::std::unique_ptr<AST::Expr> left,
      ::std::vector<AST::Attribute> outer_attrs ATTRIBUTE_UNUSED, ParseRestrictions restrictions) {
        // parse RHS (as tok has already been consumed in parse_expression)
        ::std::unique_ptr<AST::Expr> right
          = parse_expr(LBP_MINUS_ASSIG - 1, ::std::vector<AST::Attribute>(), restrictions);
        if (right == NULL)
            return NULL;
        // FIXME: ensure right-associativity for this - 'LBP - 1' may do this?

        // TODO: check types. actually, do so during semantic analysis
        location_t locus = left->get_locus_slow();

        return ::std::unique_ptr<AST::CompoundAssignmentExpr>(new AST::CompoundAssignmentExpr(
          ::std::move(left), ::std::move(right), AST::CompoundAssignmentExpr::SUBTRACT, locus));
    }

    // Parses a binary multiplication-assignment expression (with Pratt parsing).
    ::std::unique_ptr<AST::CompoundAssignmentExpr> Parser::parse_mult_assig_expr(
      const_TokenPtr tok ATTRIBUTE_UNUSED, ::std::unique_ptr<AST::Expr> left,
      ::std::vector<AST::Attribute> outer_attrs ATTRIBUTE_UNUSED, ParseRestrictions restrictions) {
        // parse RHS (as tok has already been consumed in parse_expression)
        ::std::unique_ptr<AST::Expr> right
          = parse_expr(LBP_MULT_ASSIG - 1, ::std::vector<AST::Attribute>(), restrictions);
        if (right == NULL)
            return NULL;
        // FIXME: ensure right-associativity for this - 'LBP - 1' may do this?

        // TODO: check types. actually, do so during semantic analysis
        location_t locus = left->get_locus_slow();

        return ::std::unique_ptr<AST::CompoundAssignmentExpr>(new AST::CompoundAssignmentExpr(
          ::std::move(left), ::std::move(right), AST::CompoundAssignmentExpr::MULTIPLY, locus));
    }

    // Parses a binary division-assignment expression (with Pratt parsing).
    ::std::unique_ptr<AST::CompoundAssignmentExpr> Parser::parse_div_assig_expr(
      const_TokenPtr tok ATTRIBUTE_UNUSED, ::std::unique_ptr<AST::Expr> left,
      ::std::vector<AST::Attribute> outer_attrs ATTRIBUTE_UNUSED, ParseRestrictions restrictions) {
        // parse RHS (as tok has already been consumed in parse_expression)
        ::std::unique_ptr<AST::Expr> right
          = parse_expr(LBP_DIV_ASSIG - 1, ::std::vector<AST::Attribute>(), restrictions);
        if (right == NULL)
            return NULL;
        // FIXME: ensure right-associativity for this - 'LBP - 1' may do this?

        // TODO: check types. actually, do so during semantic analysis
        location_t locus = left->get_locus_slow();

        return ::std::unique_ptr<AST::CompoundAssignmentExpr>(new AST::CompoundAssignmentExpr(
          ::std::move(left), ::std::move(right), AST::CompoundAssignmentExpr::DIVIDE, locus));
    }

    // Parses a binary modulo-assignment expression (with Pratt parsing).
    ::std::unique_ptr<AST::CompoundAssignmentExpr> Parser::parse_mod_assig_expr(
      const_TokenPtr tok ATTRIBUTE_UNUSED, ::std::unique_ptr<AST::Expr> left,
      ::std::vector<AST::Attribute> outer_attrs ATTRIBUTE_UNUSED, ParseRestrictions restrictions) {
        // parse RHS (as tok has already been consumed in parse_expression)
        ::std::unique_ptr<AST::Expr> right
          = parse_expr(LBP_MOD_ASSIG - 1, ::std::vector<AST::Attribute>(), restrictions);
        if (right == NULL)
            return NULL;
        // FIXME: ensure right-associativity for this - 'LBP - 1' may do this?

        // TODO: check types. actually, do so during semantic analysis
        location_t locus = left->get_locus_slow();

        return ::std::unique_ptr<AST::CompoundAssignmentExpr>(new AST::CompoundAssignmentExpr(
          ::std::move(left), ::std::move(right), AST::CompoundAssignmentExpr::MODULUS, locus));
    }

    // Parses a binary and-assignment expression (with Pratt parsing).
    ::std::unique_ptr<AST::CompoundAssignmentExpr> Parser::parse_and_assig_expr(
      const_TokenPtr tok ATTRIBUTE_UNUSED, ::std::unique_ptr<AST::Expr> left,
      ::std::vector<AST::Attribute> outer_attrs ATTRIBUTE_UNUSED, ParseRestrictions restrictions) {
        // parse RHS (as tok has already been consumed in parse_expression)
        ::std::unique_ptr<AST::Expr> right
          = parse_expr(LBP_AMP_ASSIG - 1, ::std::vector<AST::Attribute>(), restrictions);
        if (right == NULL)
            return NULL;
        // FIXME: ensure right-associativity for this - 'LBP - 1' may do this?

        // TODO: check types. actually, do so during semantic analysis
        location_t locus = left->get_locus_slow();

        return ::std::unique_ptr<AST::CompoundAssignmentExpr>(new AST::CompoundAssignmentExpr(
          ::std::move(left), ::std::move(right), AST::CompoundAssignmentExpr::BITWISE_AND, locus));
    }

    // Parses a binary or-assignment expression (with Pratt parsing).
    ::std::unique_ptr<AST::CompoundAssignmentExpr> Parser::parse_or_assig_expr(
      const_TokenPtr tok ATTRIBUTE_UNUSED, ::std::unique_ptr<AST::Expr> left,
      ::std::vector<AST::Attribute> outer_attrs ATTRIBUTE_UNUSED, ParseRestrictions restrictions) {
        // parse RHS (as tok has already been consumed in parse_expression)
        ::std::unique_ptr<AST::Expr> right
          = parse_expr(LBP_PIPE_ASSIG - 1, ::std::vector<AST::Attribute>(), restrictions);
        if (right == NULL)
            return NULL;
        // FIXME: ensure right-associativity for this - 'LBP - 1' may do this?

        // TODO: check types. actually, do so during semantic analysis
        location_t locus = left->get_locus_slow();

        return ::std::unique_ptr<AST::CompoundAssignmentExpr>(new AST::CompoundAssignmentExpr(
          ::std::move(left), ::std::move(right), AST::CompoundAssignmentExpr::BITWISE_OR, locus));
    }

    // Parses a binary xor-assignment expression (with Pratt parsing).
    ::std::unique_ptr<AST::CompoundAssignmentExpr> Parser::parse_xor_assig_expr(
      const_TokenPtr tok ATTRIBUTE_UNUSED, ::std::unique_ptr<AST::Expr> left,
      ::std::vector<AST::Attribute> outer_attrs ATTRIBUTE_UNUSED, ParseRestrictions restrictions) {
        // parse RHS (as tok has already been consumed in parse_expression)
        ::std::unique_ptr<AST::Expr> right
          = parse_expr(LBP_CARET_ASSIG - 1, ::std::vector<AST::Attribute>(), restrictions);
        if (right == NULL)
            return NULL;
        // FIXME: ensure right-associativity for this - 'LBP - 1' may do this?

        // TODO: check types. actually, do so during semantic analysis
        location_t locus = left->get_locus_slow();

        return ::std::unique_ptr<AST::CompoundAssignmentExpr>(new AST::CompoundAssignmentExpr(
          ::std::move(left), ::std::move(right), AST::CompoundAssignmentExpr::BITWISE_XOR, locus));
    }

    // Parses a binary left shift-assignment expression (with Pratt parsing).
    ::std::unique_ptr<AST::CompoundAssignmentExpr> Parser::parse_left_shift_assig_expr(
      const_TokenPtr tok ATTRIBUTE_UNUSED, ::std::unique_ptr<AST::Expr> left,
      ::std::vector<AST::Attribute> outer_attrs ATTRIBUTE_UNUSED, ParseRestrictions restrictions) {
        // parse RHS (as tok has already been consumed in parse_expression)
        ::std::unique_ptr<AST::Expr> right
          = parse_expr(LBP_L_SHIFT_ASSIG - 1, ::std::vector<AST::Attribute>(), restrictions);
        if (right == NULL)
            return NULL;
        // FIXME: ensure right-associativity for this - 'LBP - 1' may do this?

        // TODO: check types. actually, do so during semantic analysis
        location_t locus = left->get_locus_slow();

        return ::std::unique_ptr<AST::CompoundAssignmentExpr>(new AST::CompoundAssignmentExpr(
          ::std::move(left), ::std::move(right), AST::CompoundAssignmentExpr::LEFT_SHIFT, locus));
    }

    // Parses a binary right shift-assignment expression (with Pratt parsing).
    ::std::unique_ptr<AST::CompoundAssignmentExpr> Parser::parse_right_shift_assig_expr(
      const_TokenPtr tok ATTRIBUTE_UNUSED, ::std::unique_ptr<AST::Expr> left,
      ::std::vector<AST::Attribute> outer_attrs ATTRIBUTE_UNUSED, ParseRestrictions restrictions) {
        // parse RHS (as tok has already been consumed in parse_expression)
        ::std::unique_ptr<AST::Expr> right
          = parse_expr(LBP_R_SHIFT_ASSIG - 1, ::std::vector<AST::Attribute>(), restrictions);
        if (right == NULL)
            return NULL;
        // FIXME: ensure right-associativity for this - 'LBP - 1' may do this?

        // TODO: check types. actually, do so during semantic analysis
        location_t locus = left->get_locus_slow();

        return ::std::unique_ptr<AST::CompoundAssignmentExpr>(new AST::CompoundAssignmentExpr(
          ::std::move(left), ::std::move(right), AST::CompoundAssignmentExpr::RIGHT_SHIFT, locus));
    }

    // Parses a postfix unary await expression (with Pratt parsing).
    ::std::unique_ptr<AST::AwaitExpr> Parser::parse_await_expr(const_TokenPtr tok,
      ::std::unique_ptr<AST::Expr> expr_to_await, ::std::vector<AST::Attribute> outer_attrs) {
        // skip "await" identifier (as "." has already been consumed in parse_expression)
        // this assumes that the identifier was already identified as await
        if (!skip_token(IDENTIFIER)) {
            error_at(tok->get_locus(),
              "failed to skip 'await' in await expr - this is probably a deep issue.");
            // skip somewhere?
            return NULL;
        }

        // TODO: check inside async block in semantic analysis
        location_t locus = expr_to_await->get_locus_slow();

        return ::std::unique_ptr<AST::AwaitExpr>(
          new AST::AwaitExpr(::std::move(expr_to_await), ::std::move(outer_attrs), locus));
    }

    /* Parses an exclusive range ('..') in left denotation position (i.e. RangeFromExpr or
     * RangeFromToExpr). */
    ::std::unique_ptr<AST::RangeExpr> Parser::parse_led_range_exclusive_expr(
      const_TokenPtr tok ATTRIBUTE_UNUSED, ::std::unique_ptr<AST::Expr> left,
      ::std::vector<AST::Attribute> outer_attrs ATTRIBUTE_UNUSED, ParseRestrictions restrictions) {
        // FIXME: this probably parses expressions accidently or whatever
        // try parsing RHS (as tok has already been consumed in parse_expression)
        ::std::unique_ptr<AST::Expr> right
          = parse_expr(LBP_DOT_DOT, ::std::vector<AST::Attribute>(), restrictions);

        location_t locus = left->get_locus_slow();

        if (right == NULL) {
            // range from expr
            return ::std::unique_ptr<AST::RangeFromExpr>(
              new AST::RangeFromExpr(::std::move(left), locus));
        } else {
            return ::std::unique_ptr<AST::RangeFromToExpr>(
              new AST::RangeFromToExpr(::std::move(left), ::std::move(right), locus));
        }
        // FIXME: make non-associative
    }

    /* Parses an exclusive range ('..') in null denotation position (i.e. RangeToExpr or
     * RangeFullExpr). */
    ::std::unique_ptr<AST::RangeExpr> Parser::parse_nud_range_exclusive_expr(
      const_TokenPtr tok, ::std::vector<AST::Attribute> outer_attrs ATTRIBUTE_UNUSED) {
        // FIXME: this probably parses expressions accidently or whatever
        // try parsing RHS (as tok has already been consumed in parse_expression)
        ::std::unique_ptr<AST::Expr> right = parse_expr(LBP_DOT_DOT, ::std::vector<AST::Attribute>());

        location_t locus = tok->get_locus();

        if (right == NULL) {
            // range from expr
            return ::std::unique_ptr<AST::RangeFullExpr>(new AST::RangeFullExpr(locus));
        } else {
            return ::std::unique_ptr<AST::RangeToExpr>(
              new AST::RangeToExpr(::std::move(right), locus));
        }
        // FIXME: make non-associative
    }

    // Parses a full binary range inclusive expression.
    ::std::unique_ptr<AST::RangeFromToInclExpr> Parser::parse_range_inclusive_expr(
      const_TokenPtr tok ATTRIBUTE_UNUSED, ::std::unique_ptr<AST::Expr> left,
      ::std::vector<AST::Attribute> outer_attrs ATTRIBUTE_UNUSED, ParseRestrictions restrictions) {
        // parse RHS (as tok has already been consumed in parse_expression)
        ::std::unique_ptr<AST::Expr> right
          = parse_expr(LBP_DOT_DOT_EQ, ::std::vector<AST::Attribute>(), restrictions);
        if (right == NULL)
            return NULL;
        // FIXME: make non-associative

        // TODO: check types. actually, do so during semantic analysis
        location_t locus = left->get_locus_slow();

        return ::std::unique_ptr<AST::RangeFromToInclExpr>(
          new AST::RangeFromToInclExpr(::std::move(left), ::std::move(right), locus));
    }

    // Parses an inclusive range-to prefix unary expression.
    ::std::unique_ptr<AST::RangeToInclExpr> Parser::parse_range_to_inclusive_expr(
      const_TokenPtr tok, ::std::vector<AST::Attribute> outer_attrs ATTRIBUTE_UNUSED) {
        // parse RHS (as tok has already been consumed in parse_expression)
        ::std::unique_ptr<AST::Expr> right = parse_expr(LBP_DOT_DOT_EQ);
        if (right == NULL)
            return NULL;
        // FIXME: make non-associative

        // TODO: check types. actually, do so during semantic analysis

        return ::std::unique_ptr<AST::RangeToInclExpr>(
          new AST::RangeToInclExpr(::std::move(right), tok->get_locus()));
    }

    // Parses a pseudo-binary infix tuple index expression.
    ::std::unique_ptr<AST::TupleIndexExpr> Parser::parse_tuple_index_expr(
      const_TokenPtr tok ATTRIBUTE_UNUSED, ::std::unique_ptr<AST::Expr> tuple_expr,
      ::std::vector<AST::Attribute> outer_attrs, ParseRestrictions restrictions ATTRIBUTE_UNUSED) {
        // parse int literal (as token already skipped)
        const_TokenPtr index_tok = expect_token(INT_LITERAL);
        if (index_tok == NULL) {
            return NULL;
        }
        ::std::string index = index_tok->get_str();

        // convert to integer
        int index_int = atoi(index.c_str());

        location_t locus = tuple_expr->get_locus_slow();

        return ::std::unique_ptr<AST::TupleIndexExpr>(new AST::TupleIndexExpr(
          ::std::move(tuple_expr), index_int, ::std::move(outer_attrs), locus));
    }

    // Parses a pseudo-binary infix array (or slice) index expression.
    ::std::unique_ptr<AST::ArrayIndexExpr> Parser::parse_index_expr(
      const_TokenPtr tok ATTRIBUTE_UNUSED, ::std::unique_ptr<AST::Expr> array_expr,
      ::std::vector<AST::Attribute> outer_attrs, ParseRestrictions restrictions) {
        // parse RHS (as tok has already been consumed in parse_expression)
        ::std::unique_ptr<AST::Expr> index_expr
          = parse_expr(LBP_ARRAY_REF, ::std::vector<AST::Attribute>(), restrictions);
        if (index_expr == NULL)
            return NULL;

        // skip ']' at end of array
        if (!skip_token(RIGHT_SQUARE)) {
            // skip somewhere?
            return NULL;
        }

        // TODO: check types. actually, do so during semantic analysis
        location_t locus = array_expr->get_locus_slow();

        return ::std::unique_ptr<AST::ArrayIndexExpr>(new AST::ArrayIndexExpr(
          ::std::move(array_expr), ::std::move(index_expr), ::std::move(outer_attrs), locus));
    }

    // Parses a pseudo-binary infix struct field access expression.
    ::std::unique_ptr<AST::FieldAccessExpr> Parser::parse_field_access_expr(
      const_TokenPtr tok ATTRIBUTE_UNUSED, ::std::unique_ptr<AST::Expr> struct_expr,
      ::std::vector<AST::Attribute> outer_attrs, ParseRestrictions restrictions ATTRIBUTE_UNUSED) {
        // get field name identifier (assume that this is a field access expr and not say await)
        const_TokenPtr ident_tok = expect_token(IDENTIFIER);
        Identifier ident = ident_tok->get_str();

        location_t locus = struct_expr->get_locus_slow();

        // TODO: check types. actually, do so during semantic analysis
        return ::std::unique_ptr<AST::FieldAccessExpr>(new AST::FieldAccessExpr(
          ::std::move(struct_expr), ::std::move(ident), ::std::move(outer_attrs), locus));
    }

    // Parses a pseudo-binary infix method call expression.
    ::std::unique_ptr<AST::MethodCallExpr> Parser::parse_method_call_expr(const_TokenPtr tok,
      ::std::unique_ptr<AST::Expr> receiver_expr, ::std::vector<AST::Attribute> outer_attrs,
      ParseRestrictions restrictions ATTRIBUTE_UNUSED) {
        // parse path expr segment
        AST::PathExprSegment segment = parse_path_expr_segment();
        if (segment.is_error()) {
            error_at(tok->get_locus(), "failed to parse path expr segment of method call expr");
            return NULL;
        }

        // skip left parentheses
        if (!skip_token(LEFT_PAREN)) {
            return NULL;
        }

        // parse method params (if they exist)
        ::std::vector< ::std::unique_ptr<AST::Expr> > params;

        const_TokenPtr t = lexer.peek_token();
        while (t->get_id() != RIGHT_PAREN) {
            ::std::unique_ptr<AST::Expr> param = parse_expr();
            if (param == NULL) {
                error_at(t->get_locus(), "failed to parse method param in method call");
                return NULL;
            }
            params.push_back(::std::move(param));

            if (lexer.peek_token()->get_id() != COMMA) {
                break;
            }
            lexer.skip_token();

            t = lexer.peek_token();
        }

        // skip right paren
        if (!skip_token(RIGHT_PAREN)) {
            return NULL;
        }

        // TODO: check types. actually do so in semantic analysis pass.
        location_t locus = receiver_expr->get_locus_slow();

        return ::std::unique_ptr<AST::MethodCallExpr>(
          new AST::MethodCallExpr(::std::move(receiver_expr), ::std::move(segment),
            ::std::move(params), ::std::move(outer_attrs), locus));
    }

    // Parses a pseudo-binary infix function call expression.
    ::std::unique_ptr<AST::CallExpr> Parser::parse_function_call_expr(
      const_TokenPtr tok ATTRIBUTE_UNUSED, ::std::unique_ptr<AST::Expr> function_expr,
      ::std::vector<AST::Attribute> outer_attrs, ParseRestrictions restrictions ATTRIBUTE_UNUSED) {
        // parse function params (if they exist)
        ::std::vector< ::std::unique_ptr<AST::Expr> > params;

        const_TokenPtr t = lexer.peek_token();
        while (t->get_id() != RIGHT_PAREN) {
            ::std::unique_ptr<AST::Expr> param = parse_expr();
            if (param == NULL) {
                error_at(t->get_locus(), "failed to parse function param in function call");
                return NULL;
            }
            params.push_back(::std::move(param));

            if (lexer.peek_token()->get_id() != COMMA) {
                break;
            }
            lexer.skip_token();

            t = lexer.peek_token();
        }

        // skip ')' at end of param list
        if (!skip_token(RIGHT_PAREN)) {
            // skip somewhere?
            return NULL;
        }

        // TODO: check types. actually, do so during semantic analysis
        location_t locus = function_expr->get_locus_slow();

        return ::std::unique_ptr<AST::CallExpr>(new AST::CallExpr(
          ::std::move(function_expr), ::std::move(params), ::std::move(outer_attrs), locus));
    }

    // Parses a macro invocation with a path in expression already parsed (but not '!' token).
    ::std::unique_ptr<AST::MacroInvocation> Parser::parse_macro_invocation_partial(
      AST::PathInExpression path, ::std::vector<AST::Attribute> outer_attrs) {
        // macro invocation
        if (!skip_token(EXCLAM)) {
            return NULL;
        }

        // convert PathInExpression to SimplePath - if this isn't possible, error
        AST::SimplePath converted_path = path.as_simple_path();
        if (converted_path.is_empty()) {
            error_at(
              lexer.peek_token()->get_locus(), "failed to parse simple path in macro invocation");
            return NULL;
        }

        AST::DelimTokenTree tok_tree = parse_delim_token_tree();

        fprintf(stderr, "successfully parsed macro invocation (via partial)\n");

        location_t macro_locus = converted_path.get_locus();

        return ::std::unique_ptr<AST::MacroInvocation>(new AST::MacroInvocation(
          ::std::move(converted_path), ::std::move(tok_tree), ::std::move(outer_attrs), macro_locus));
    }

    // Parses a struct expr struct with a path in expression already parsed (but not '{' token).
    ::std::unique_ptr<AST::StructExprStruct> Parser::parse_struct_expr_struct_partial(
      AST::PathInExpression path, ::std::vector<AST::Attribute> outer_attrs) {
        // assume struct expr struct (as struct-enum disambiguation requires name
        // lookup) again, make statement if final ';'
        if (!skip_token(LEFT_CURLY)) {
            return NULL;
        }

        // parse inner attributes
        ::std::vector<AST::Attribute> inner_attrs = parse_inner_attributes();

        // branch based on next token
        const_TokenPtr t = lexer.peek_token();
        location_t path_locus = path.get_locus();
        switch (t->get_id()) {
            case RIGHT_CURLY:
                // struct with no body
                lexer.skip_token();

                return ::std::unique_ptr<AST::StructExprStruct>(new AST::StructExprStruct(
                  ::std::move(path), ::std::move(inner_attrs), ::std::move(outer_attrs), path_locus));
            case DOT_DOT:
                /* technically this would give a struct base-only struct, but this
                 * algorithm should work too. As such, AST type not happening. */
            case IDENTIFIER:
            case INT_LITERAL: {
                // struct with struct expr fields

                // parse struct expr fields
                ::std::vector< ::std::unique_ptr<AST::StructExprField> > fields;

                while (t->get_id() != RIGHT_CURLY && t->get_id() != DOT_DOT) {
                    ::std::unique_ptr<AST::StructExprField> field = parse_struct_expr_field();
                    if (field == NULL) {
                        error_at(t->get_locus(), "failed to parse struct (or enum) expr field");
                        return NULL;
                    }

                    // DEBUG:
                    fprintf(stderr, "struct/enum expr field validated to not be null \n");

                    fields.push_back(::std::move(field));

                    // DEBUG:
                    fprintf(stderr, "struct/enum expr field pushed back \n");

                    if (lexer.peek_token()->get_id() != COMMA) {
                        // DEBUG:
                        fprintf(
                          stderr, "lack of comma detected in struct/enum expr fields - break \n");
                        break;
                    }
                    lexer.skip_token();

                    // DEBUG:
                    fprintf(stderr, "struct/enum expr fields comma skipped \n");

                    t = lexer.peek_token();
                }

                // DEBUG:
                fprintf(stderr, "struct/enum expr about to parse struct base \n");

                // parse struct base if it exists
                AST::StructBase struct_base = AST::StructBase::error();
                if (lexer.peek_token()->get_id() == DOT_DOT) {
                    lexer.skip_token();

                    // parse required struct base expr
                    ::std::unique_ptr<AST::Expr> base_expr = parse_expr();
                    if (base_expr == NULL) {
                        error_at(lexer.peek_token()->get_locus(),
                          "failed to parse struct base expression in struct "
                          "expression");
                        return NULL;
                    }

                    // DEBUG:
                    fprintf(stderr, "struct/enum expr - parsed and validated base expr \n");

                    struct_base = AST::StructBase(::std::move(base_expr));

                    // DEBUG:
                    fprintf(stderr, "assigned struct base to new struct base \n");
                }

                if (!skip_token(RIGHT_CURLY)) {
                    return NULL;
                }

                // DEBUG:
                fprintf(stderr, "struct/enum expr skipped right curly - done and ready to return \n");

                return ::std::unique_ptr<AST::StructExprStructFields>(
                  new AST::StructExprStructFields(::std::move(path), ::std::move(fields), path_locus,
                    ::std::move(struct_base), ::std::move(inner_attrs), ::std::move(outer_attrs)));
            }
            default:
                error_at(t->get_locus(),
                  "unrecognised token '%s' in struct (or enum) expression - "
                  "expected '}', identifier, int literal, or '..'",
                  t->get_token_description());
                return NULL;
        }
    }

    // Parses a struct expr tuple with a path in expression already parsed (but not '(' token).
    ::std::unique_ptr<AST::StructExprTuple> Parser::parse_struct_expr_tuple_partial(
      AST::PathInExpression path, ::std::vector<AST::Attribute> outer_attrs) {
        if (!skip_token(LEFT_PAREN)) {
            return NULL;
        }

        ::std::vector<AST::Attribute> inner_attrs = parse_inner_attributes();

        ::std::vector< ::std::unique_ptr<AST::Expr> > exprs;

        const_TokenPtr t = lexer.peek_token();
        while (t->get_id() != RIGHT_PAREN) {
            // parse expression (required)
            ::std::unique_ptr<AST::Expr> expr = parse_expr();
            if (expr == NULL) {
                error_at(t->get_locus(), "failed to parse expression in struct "
                                         "(or enum) expression tuple");
                return NULL;
            }
            exprs.push_back(::std::move(expr));

            if (lexer.peek_token()->get_id() != COMMA) {
                break;
            }
            lexer.skip_token();

            t = lexer.peek_token();
        }

        if (!skip_token(RIGHT_PAREN)) {
            return NULL;
        }

        location_t path_locus = path.get_locus();

        return ::std::unique_ptr<AST::StructExprTuple>(new AST::StructExprTuple(::std::move(path),
          ::std::move(exprs), ::std::move(inner_attrs), ::std::move(outer_attrs), path_locus));
    }

    /* Parses a path in expression with the first token passed as a parameter (as it is skipped in
     * token stream). Note that this only parses segment-first paths, not global ones. */
    AST::PathInExpression Parser::parse_path_in_expression_pratt(const_TokenPtr tok) {
        // HACK-y way of making up for pratt-parsing consuming first token

        // DEBUG
        fprintf(stderr, "current peek token when starting path pratt parse: '%s'\n",
          lexer.peek_token()->get_token_description());

        // create segment vector
        ::std::vector<AST::PathExprSegment> segments;

        ::std::string initial_str;

        switch (tok->get_id()) {
            case IDENTIFIER:
                initial_str = tok->get_str();
                break;
            case SUPER:
                initial_str = "super";
                break;
            case SELF:
                initial_str = "self";
                break;
            case SELF_ALIAS:
                initial_str = "Self";
                break;
            case CRATE:
                initial_str = "crate";
                break;
            case DOLLAR_SIGN:
                if (lexer.peek_token()->get_id() == CRATE) {
                    initial_str = "$crate";
                    break;
                }
                gcc_fallthrough();
            default:
                error_at(tok->get_locus(), "unrecognised token '%s' in path in expression",
                  tok->get_token_description());
                return AST::PathInExpression::create_error();
        }

        // parse required initial segment
        AST::PathExprSegment initial_segment(initial_str, tok->get_locus());
        // parse generic args (and turbofish), if they exist
        /* use lookahead to determine if they actually exist (don't want to accidently parse
         * over next ident segment) */
        if (lexer.peek_token()->get_id() == SCOPE_RESOLUTION
            && lexer.peek_token(1)->get_id() == LEFT_ANGLE) {
            // skip scope resolution
            lexer.skip_token();

            AST::GenericArgs generic_args = parse_path_generic_args();

            initial_segment
              = AST::PathExprSegment(initial_str, tok->get_locus(), ::std::move(generic_args));
        }
        if (initial_segment.is_error()) {
            // skip after somewhere?
            // don't necessarily throw error but yeah

            // DEBUG
            fprintf(stderr, "initial segment is error - returning null\n");

            return AST::PathInExpression::create_error();
        }
        segments.push_back(::std::move(initial_segment));

        // parse optional segments (as long as scope resolution operator exists)
        const_TokenPtr t = lexer.peek_token();
        while (t->get_id() == SCOPE_RESOLUTION) {
            // skip scope resolution operator
            lexer.skip_token();

            // parse the actual segment - it is an error if it doesn't exist now
            AST::PathExprSegment segment = parse_path_expr_segment();
            if (segment.is_error()) {
                // skip after somewhere?
                error_at(t->get_locus(), "could not parse path expression segment");
                return AST::PathInExpression::create_error();
            }

            segments.push_back(::std::move(segment));

            t = lexer.peek_token();
        }

        // DEBUG:
        fprintf(stderr, "current token (just about to return path to null denotation): '%s'\n",
          lexer.peek_token()->get_token_description());

        return AST::PathInExpression(::std::move(segments), tok->get_locus(), false);
    }

    // Parses a closure expression with pratt parsing (from null denotation).
    ::std::unique_ptr<AST::ClosureExpr> Parser::parse_closure_expr_pratt(
      const_TokenPtr tok, ::std::vector<AST::Attribute> outer_attrs) {
        // TODO: does this need pratt parsing (for precedence)? probably not, but idk
        location_t locus = tok->get_locus();
        bool has_move = false;
        if (tok->get_id() == MOVE) {
            has_move = true;
            tok = lexer.peek_token();
            lexer.skip_token();
            // skip token and reassign
        }

        // handle parameter list
        ::std::vector<AST::ClosureParam> params;

        switch (tok->get_id()) {
            case OR:
                // no parameters, don't skip token
                break;
            case PIPE: {
                // actually may have parameters
                // don't skip token
                const_TokenPtr t = lexer.peek_token();
                while (t->get_id() != PIPE) {
                    AST::ClosureParam param = parse_closure_param();
                    if (param.is_error()) {
                        // TODO is this really an error?
                        error_at(t->get_locus(), "could not parse closure param");
                        return NULL;
                    }
                    params.push_back(::std::move(param));

                    if (lexer.peek_token()->get_id() != COMMA) {
                        // not an error but means param list is done
                        break;
                    }
                    // skip comma
                    lexer.skip_token();

                    t = lexer.peek_token();
                }

                if (!skip_token(PIPE)) {
                    return NULL;
                }
                break;
            }
            default:
                error_at(tok->get_locus(),
                  "unexpected token '%s' in closure expression - expected '|' or '||'",
                  tok->get_token_description());
                // skip somewhere?
                return NULL;
        }

        // again branch based on next token
        tok = lexer.peek_token();
        if (tok->get_id() == RETURN_TYPE) {
            // must be return type closure with block expr

            // skip "return type" token
            lexer.skip_token();

            // parse actual type, which is required
            ::std::unique_ptr<AST::TypeNoBounds> type = parse_type_no_bounds();
            if (type == NULL) {
                // error
                error_at(tok->get_locus(), "failed to parse type for closure");
                // skip somewhere?
                return NULL;
            }

            // parse block expr, which is required
            ::std::unique_ptr<AST::BlockExpr> block = parse_block_expr();
            if (block == NULL) {
                // error
                error_at(lexer.peek_token()->get_locus(), "failed to parse block expr in closure");
                // skip somewhere?
                return NULL;
            }

            return ::std::unique_ptr<AST::ClosureExprInnerTyped>(
              new AST::ClosureExprInnerTyped(::std::move(type), ::std::move(block),
                ::std::move(params), locus, has_move, ::std::move(outer_attrs)));
        } else {
            // must be expr-only closure

            // parse expr, which is required
            ::std::unique_ptr<AST::Expr> expr = parse_expr();
            if (expr == NULL) {
                error_at(tok->get_locus(), "failed to parse expression in closure");
                // skip somewhere?
                return NULL;
            }

            return ::std::unique_ptr<AST::ClosureExprInner>(new AST::ClosureExprInner(
              ::std::move(expr), ::std::move(params), locus, has_move, ::std::move(outer_attrs)));
        }
    }

    /* Parses a tuple index expression (pratt-parsed) from a 'float' token as a result of lexer
     * misidentification. */
    ::std::unique_ptr<AST::TupleIndexExpr> Parser::parse_tuple_index_expr_float(const_TokenPtr tok,
      ::std::unique_ptr<AST::Expr> tuple_expr, ::std::vector<AST::Attribute> outer_attrs,
      ParseRestrictions restrictions ATTRIBUTE_UNUSED) {
        // only works on float literals
        if (tok->get_id() != FLOAT_LITERAL) {
            return NULL;
        }

        // DEBUG:
        fprintf(stderr, "exact string form of float: '%s'\n", tok->get_str().c_str());

        // get float string and remove dot and initial 0
        ::std::string index_str = tok->get_str();
        index_str.erase(index_str.begin());

        // get int from string
        int index = atoi(index_str.c_str());

        location_t locus = tuple_expr->get_locus_slow();

        return ::std::unique_ptr<AST::TupleIndexExpr>(
          new AST::TupleIndexExpr(::std::move(tuple_expr), index, ::std::move(outer_attrs), locus));
    }

    // Determines action to take when finding token at beginning of expression.
    Tree Parser::null_denotation(const_TokenPtr tok) {
        // note: tok is previous character in input stream, not current one, as parse_expression
        // skips it before passing it in

        /* as a Pratt parser (which works by decomposing expressions into a null denotation and then a
         * left denotation), null denotations handle primaries and unary operands (but only prefix
         * unary operands) */

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
            case MINUS: { // unary minus - TODO: does not work on unsigned integers
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
            case EXCLAM: { // logical not - TODO: this could also be bitwise not
                Tree expr = parse_expression(LBP_UNARY_EXCLAM /*LOGICAL_NOT*/);

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
            case ASTERISK: {
                // TODO: fix: this is pointer dereference only, I think
                Tree expr = parse_expression(LBP_UNARY_ASTERISK);
                return expr;
            }
            case AMP: {
                // TODO: fix: this is reference only, I think
                Tree expr = NULL_TREE;

                if (lexer.peek_token()->get_id() == MUT)
                    expr = parse_expression(LBP_UNARY_AMP_MUT);
                else
                    expr = parse_expression(LBP_UNARY_AMP);

                return expr;
            }
            case SCOPE_RESOLUTION: {
                // TODO: fix: this is for global paths, i.e. ::std::string::whatever
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
    Tree Parser::left_denotation(const_TokenPtr tok ATTRIBUTE_UNUSED, Tree left ATTRIBUTE_UNUSED) {
        /*BinaryHandler binary_handler = get_binary_handler(tok->get_id());
        if (binary_handler == NULL) {
            unexpected_token(tok);
            return Tree::error();
        }

        return (this->*binary_handler)(tok, left);*/
        return Tree::error();
    }

    // Gets method for handling binary operation parsing for specific token type.
    /*Parser::BinaryHandler Parser::get_binary_handler(TokenId id) {
        switch (id) {
#define BINARY_HANDLER(name, token_id) \
    case token_id:                     \
        return &Parser::binary_##name;
            BINARY_HANDLER_LIST
#undef BINARY_HANDLER
            default:
                return NULL;
        }
    }*/

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
    Tree Parser::binary_not_equal /*different*/ (const_TokenPtr tok, Tree left) {
        // parse RHS
        Tree right = parse_expression(LBP_NOT_EQUAL);
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
    Tree Parser::binary_array_index(const_TokenPtr tok, Tree left) {
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

    // Method stub
    Tree Parser::binary_bitwise_or(const_TokenPtr tok ATTRIBUTE_UNUSED, Tree left ATTRIBUTE_UNUSED) {
        return NULL_TREE;
    }

    // Method stub
    Tree Parser::binary_left_shift(const_TokenPtr tok ATTRIBUTE_UNUSED, Tree left ATTRIBUTE_UNUSED) {
        return NULL_TREE;
    }

    // Method stub
    Tree Parser::binary_bitwise_and(const_TokenPtr tok ATTRIBUTE_UNUSED, Tree left ATTRIBUTE_UNUSED) {
        return NULL_TREE;
    }

    // Method stub
    Tree Parser::binary_bitwise_xor(const_TokenPtr tok ATTRIBUTE_UNUSED, Tree left ATTRIBUTE_UNUSED) {
        return NULL_TREE;
    }

    // Method stub
    Tree Parser::binary_right_shift(const_TokenPtr tok ATTRIBUTE_UNUSED, Tree left ATTRIBUTE_UNUSED) {
        return NULL_TREE;
    }

    // Method stub
    Tree Parser::binary_as_cast(const_TokenPtr tok ATTRIBUTE_UNUSED, Tree left ATTRIBUTE_UNUSED) {
        return NULL_TREE;
    }

    // Method stub
    Tree Parser::binary_div_assig(const_TokenPtr tok ATTRIBUTE_UNUSED, Tree left ATTRIBUTE_UNUSED) {
        return NULL_TREE;
    }

    // Method stub
    Tree Parser::binary_mod_assig(const_TokenPtr tok ATTRIBUTE_UNUSED, Tree left ATTRIBUTE_UNUSED) {
        return NULL_TREE;
    }

    // Method stub
    Tree Parser::binary_mult_assig(const_TokenPtr tok ATTRIBUTE_UNUSED, Tree left ATTRIBUTE_UNUSED) {
        return NULL_TREE;
    }

    // Method stub
    Tree Parser::binary_plus_assig(const_TokenPtr tok ATTRIBUTE_UNUSED, Tree left ATTRIBUTE_UNUSED) {
        return NULL_TREE;
    }

    // Method stub
    Tree Parser::binary_minus_assig(const_TokenPtr tok ATTRIBUTE_UNUSED, Tree left ATTRIBUTE_UNUSED) {
        return NULL_TREE;
    }

    // Method stub
    Tree Parser::binary_assignment_expr(
      const_TokenPtr tok ATTRIBUTE_UNUSED, Tree left ATTRIBUTE_UNUSED) {
        return NULL_TREE;
    }

    // Method stub
    Tree Parser::binary_bitwise_or_assig(
      const_TokenPtr tok ATTRIBUTE_UNUSED, Tree left ATTRIBUTE_UNUSED) {
        return NULL_TREE;
    }

    // Method stub
    Tree Parser::binary_bitwise_xor_assig(
      const_TokenPtr tok ATTRIBUTE_UNUSED, Tree left ATTRIBUTE_UNUSED) {
        return NULL_TREE;
    }

    // Method stub
    Tree Parser::binary_bitwise_and_assig(
      const_TokenPtr tok ATTRIBUTE_UNUSED, Tree left ATTRIBUTE_UNUSED) {
        return NULL_TREE;
    }

    // Method stub
    Tree Parser::binary_left_shift_assig(
      const_TokenPtr tok ATTRIBUTE_UNUSED, Tree left ATTRIBUTE_UNUSED) {
        return NULL_TREE;
    }

    // Method stub
    Tree Parser::binary_right_shift_assig(
      const_TokenPtr tok ATTRIBUTE_UNUSED, Tree left ATTRIBUTE_UNUSED) {
        return NULL_TREE;
    }

    // Parse variable assignment statement. This is not the same as variable declaration.
    Tree Parser::parse_assignment_statement() {
        Tree variable = parse_lhs_assignment_expression();

        if (variable.is_error())
            return Tree::error();

        // TODO: fix
        const_TokenPtr assig_tok = expect_token(/*ASSIG*/ COLON);
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
    /*Tree Parser::parse_write_statement() {
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
    }*/

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

#if 0
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
#endif

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

        skip_token(RIGHT_CURLY);

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

        // TODO
        if (!skip_token(/*ASSIG*/ COLON)) {
            skip_after_end();
            return Tree::error();
        }

        // parse lower bound expression
        Tree lower_bound = parse_integer_expression();

        // TODO
        if (!skip_token(/*TO*/ DOT)) {
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

        skip_token(RIGHT_CURLY);

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
        return (t->get_id() == RIGHT_CURLY || t->get_id() == ELSE || t->get_id() == END_OF_FILE);
    }

    // Returns true if the next token is END or EOF.
    bool Parser::done_end() {
        const_TokenPtr t = lexer.peek_token();
        return (t->get_id() == RIGHT_CURLY || t->get_id() == END_OF_FILE);
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
    /*Tree Parser::parse_type_declaration() {
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
    }*/

    // Parses a record type field declaration.
    /*Tree Parser::parse_field_declaration(std::vector<std::string>& field_names) {
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
    }*/

    // TODO: remove: here to solve link errors
    Tree Parser::parse_record() {
        return Tree::error();
    }
    // Parses a record.
    /*Tree Parser::parse_record() {
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
    }*/

    // Dumps lexer output to stderr.
    void Parser::debug_dump_lex_output() {
        Rust::const_TokenPtr tok = lexer.peek_token();

        while (true) {
            bool has_text
              = tok->get_id() == Rust::IDENTIFIER || tok->get_id() == Rust::INT_LITERAL
                || tok->get_id() == Rust::FLOAT_LITERAL || tok->get_id() == Rust::STRING_LITERAL
                || tok->get_id() == Rust::CHAR_LITERAL || tok->get_id() == Rust::BYTE_STRING_LITERAL
                || tok->get_id() == Rust::BYTE_CHAR_LITERAL;

            location_t loc = tok->get_locus();

            fprintf(stderr, "<id=%s%s, %s, line=%d, col=%d>\n", tok->token_id_to_str(),
              has_text ? (std::string(", text=") + tok->get_str() + std::string(", typehint=")
                           + std::string(tok->get_type_hint_str()))
                           .c_str()
                       : "",
              LOCATION_FILE(loc), LOCATION_LINE(loc), LOCATION_COLUMN(loc));

            if (tok->get_id() == Rust::END_OF_FILE)
                break;

            lexer.skip_token();
            tok = lexer.peek_token();
        }
    }

    // Parses crate and dumps AST to stderr, recursively.
    void Parser::debug_dump_ast_output() {
        AST::Crate crate = parse_crate();

        // print crate "as string", which then calls each item as string, etc.
        fprintf(stderr, crate.as_string().c_str());
    }
}