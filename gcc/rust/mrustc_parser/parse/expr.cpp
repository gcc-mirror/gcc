/*
 * MRustC - Rust Compiler
 * - By John Hodge (Mutabah/thePowersGang)
 *
 * parse/expr.cpp
 * - Expression (i.e. code) parsing
 *
 * Start points:
 * - Parse_ExprBlockNode : Parses a block
 * - Parse_Stmt : Parse a single statement
 * - Parse_Expr0 : Parse a single expression
 */
#include "parseerror.hpp"
#include <ast/ast.hpp>
#include <ast/expr.hpp>
#include "common.hpp"
#include <iostream>
#include "tokentree.hpp"
#include "interpolated_fragment.hpp"

using AST::ExprNode;
using AST::ExprNodeP;
// TODO: Use a ProtoSpan instead of a point span?
static inline ExprNodeP mk_exprnodep(const TokenStream& lex, AST::ExprNode* en){en->set_span(lex.point_span()); return ExprNodeP(en); }
#define NEWNODE(type, ...)  mk_exprnodep(lex, new type(__VA_ARGS__))

//ExprNodeP Parse_ExprBlockNode(TokenStream& lex, bool is_unsafe=false);    // common.hpp
//ExprNodeP Parse_ExprBlockLine_WithItems(TokenStream& lex, ::std::shared_ptr<AST::Module>& local_mod, bool& add_silence_if_end);
//ExprNodeP Parse_ExprBlockLine(TokenStream& lex, bool *add_silence);
ExprNodeP Parse_ExprBlockLine_Stmt(TokenStream& lex, bool& has_semicolon);
//ExprNodeP Parse_Stmt(TokenStream& lex);   // common.hpp
ExprNodeP Parse_Stmt_Let(TokenStream& lex);
ExprNodeP Parse_Expr0(TokenStream& lex);
ExprNodeP Parse_IfStmt(TokenStream& lex);
ExprNodeP Parse_WhileStmt(TokenStream& lex, ::std::string lifetime);
ExprNodeP Parse_ForStmt(TokenStream& lex, ::std::string lifetime);
ExprNodeP Parse_Expr_Match(TokenStream& lex);
ExprNodeP Parse_Expr1(TokenStream& lex);
ExprNodeP Parse_ExprMacro(TokenStream& lex, AST::Path tok);

AST::Expr Parse_Expr(TokenStream& lex)
{
    return ::AST::Expr( Parse_Expr0(lex) );
}

AST::Expr Parse_ExprBlock(TokenStream& lex)
{
    return ::AST::Expr( Parse_ExprBlockNode(lex) );
}

ExprNodeP Parse_ExprBlockNode(TokenStream& lex, bool is_unsafe/*=false*/)
{
    TRACE_FUNCTION;
    Token   tok;

    ::std::vector<ExprNodeP> nodes;

    ::std::shared_ptr<AST::Module> local_mod;

    GET_CHECK_TOK(tok, lex, TOK_BRACE_OPEN);

    bool last_value_yielded = false;
    while( LOOK_AHEAD(lex) != TOK_BRACE_CLOSE )
    {
        last_value_yielded = false;
        DEBUG("tok = " << tok);

        // NOTE: Doc comments can appear within a function and apply to the function
        if( lex.parse_state().parent_attrs )
        {
            Parse_ParentAttrs(lex, *lex.parse_state().parent_attrs);
        }
        if( LOOK_AHEAD(lex) == TOK_BRACE_CLOSE )
            break;

        bool    add_silence_if_end = false;
        // `add_silence_if_end` indicates that the statement had a semicolon.
        auto rv = Parse_ExprBlockLine_WithItems(lex, local_mod, add_silence_if_end);
        if( rv )
        {
            // Set to TRUE if there was no semicolon after a statement
            last_value_yielded = !add_silence_if_end;
            nodes.push_back( mv$(rv) );
        }
        else {
            assert( !add_silence_if_end );
            last_value_yielded = false;
        }
    }
    GET_CHECK_TOK(tok, lex, TOK_BRACE_CLOSE);

    return NEWNODE( AST::ExprNode_Block, is_unsafe, last_value_yielded, mv$(nodes), mv$(local_mod) );
}

/// Parse a single line in a block, handling items added to the local module
///
/// - If an item was parsed, this returns an empty ExprNodeP
ExprNodeP Parse_ExprBlockLine_WithItems(TokenStream& lex, ::std::shared_ptr<AST::Module>& local_mod, bool& add_silence_if_end)
{
    Token   tok;

    auto item_attrs = Parse_ItemAttrs(lex);
    GET_TOK(tok, lex);

    // `union Ident` - contextual keyword
    if( tok.type() == TOK_IDENT && tok.str() == "union" && lex.lookahead(0) == TOK_IDENT ) {
        PUTBACK(tok, lex);
        if( !local_mod ) {
            local_mod = lex.parse_state().get_current_mod().add_anon();
        }
        Parse_Mod_Item(lex, *local_mod, mv$(item_attrs));
        return ExprNodeP();
    }

    if( tok.type() == TOK_IDENT && tok.str() == "macro_rules" && lex.lookahead(0) == TOK_EXCLAM )
    {
        // Special case - create a local module if macro_rules! is seen
        // - Allows correct scoping of defined macros
        if( !local_mod ) {
            local_mod = lex.parse_state().get_current_mod().add_anon();
        }
    }

    switch(tok.type())
    {
    // Items:
    case TOK_RWORD_PUB:
        // NOTE: Allowed, but doesn't do much
    case TOK_RWORD_TYPE:
    case TOK_RWORD_USE:
    case TOK_RWORD_EXTERN:
    case TOK_RWORD_CONST:
    case TOK_RWORD_STATIC:
    case TOK_RWORD_STRUCT:
    case TOK_RWORD_ENUM:
    case TOK_RWORD_TRAIT:
    case TOK_RWORD_IMPL:
    case TOK_RWORD_FN:
    case TOK_RWORD_MOD:
        PUTBACK(tok, lex);
        if( !local_mod ) {
            local_mod = lex.parse_state().get_current_mod().add_anon();
        }
        Parse_Mod_Item(lex, *local_mod, mv$(item_attrs));
        return ExprNodeP();
    // 'unsafe' - Check if the next token isn't a `{`, if so it's an item. Otherwise, fall through
    case TOK_RWORD_UNSAFE:
        if( LOOK_AHEAD(lex) != TOK_BRACE_OPEN )
        {
            PUTBACK(tok, lex);
            if( !local_mod ) {
                local_mod = lex.parse_state().get_current_mod().add_anon();
            }
            Parse_Mod_Item(lex, *local_mod, mv$(item_attrs));
            return ExprNodeP();
        }
        // fall
    default: {
        PUTBACK(tok, lex);
        auto rv = Parse_ExprBlockLine(lex, &add_silence_if_end);
        if( rv ) {
            rv->set_attrs( mv$(item_attrs) );
        }
        else if( item_attrs.m_items.size() > 0 ) {
            // TODO: Is this an error? - Attributes on a expression that didn't yeild a node.
            // - They should have applied to the item that was parsed?
        }
        else {
        }
        return rv;
        } break;
    }
}

/// Parse a single line from a block
///
/// Handles:
/// - Block-level constructs (with lifetime annotations)
/// - use/extern/const/let
ExprNodeP Parse_ExprBlockLine(TokenStream& lex, bool *add_silence)
{
    Token tok;
    ExprNodeP   ret;

    if( GET_TOK(tok, lex) == TOK_LIFETIME )
    {
        // Lifetimes can only precede loops... and blocks?
        ::std::string lifetime = tok.str();
        GET_CHECK_TOK(tok, lex, TOK_COLON);

        switch( GET_TOK(tok, lex) )
        {
        case TOK_RWORD_LOOP:
            return NEWNODE( AST::ExprNode_Loop, lifetime, Parse_ExprBlockNode(lex) );
        case TOK_RWORD_WHILE:
            return Parse_WhileStmt(lex, lifetime);
        case TOK_RWORD_FOR:
            return Parse_ForStmt(lex, lifetime);
        //case TOK_RWORD_IF:
        //    return Parse_IfStmt(lex);
        //case TOK_RWORD_MATCH:
        //    return Parse_Expr_Match(lex);
        //case TOK_BRACE_OPEN:
        //    PUTBACK(tok, lex);
        //    return Parse_ExprBlockNode(lex);

        default:
            throw ParseError::Unexpected(lex, tok);
        }
    }
    else
    {
        switch( tok.type() )
        {
        case TOK_INTERPOLATED_BLOCK:
            return tok.take_frag_node();
        case TOK_SEMICOLON:
            // Return a NULL expression, nothing here.
            return nullptr;

        // let binding
        case TOK_RWORD_LET:
            ret = Parse_Stmt_Let(lex);
            GET_CHECK_TOK(tok, lex, TOK_SEMICOLON);
            return ret;

        // Blocks that don't need semicolons
        // NOTE: If these are followed by a small set of tokens (`.` and `?`) then they are actually the start of an expression
        // HACK: Parse here, but if the next token is one of the set store in a TOK_INTERPOLATED_EXPR and invoke the statement parser
        case TOK_RWORD_LOOP:
            ret = NEWNODE( AST::ExprNode_Loop, "", Parse_ExprBlockNode(lex) );
            if(0)
        case TOK_RWORD_WHILE:
            ret = Parse_WhileStmt(lex, "");
            if(0)
        case TOK_RWORD_FOR:
            ret = Parse_ForStmt(lex, "");
            if(0)
        case TOK_RWORD_IF:
            ret = Parse_IfStmt(lex);
            if(0)
        case TOK_RWORD_MATCH:
            ret = Parse_Expr_Match(lex);
            if(0)
        case TOK_RWORD_UNSAFE:
            ret = Parse_ExprBlockNode(lex, true);
            if(0)
        case TOK_BRACE_OPEN:
            { PUTBACK(tok, lex); ret = Parse_ExprBlockNode(lex); }

            if( lex.lookahead(0) == TOK_DOT || lex.lookahead(0) == TOK_QMARK ) {
                lex.putback( Token(Token::TagTakeIP(), InterpolatedFragment(InterpolatedFragment::EXPR, ret.release())) );
                return Parse_ExprBlockLine_Stmt(lex, *add_silence);
            }

            if( LOOK_AHEAD(lex) == TOK_SEMICOLON ) {
                GET_TOK(tok, lex);
                *add_silence = true;
            }

            return ret;

        // Flow control
        case TOK_RWORD_RETURN:
        case TOK_RWORD_CONTINUE:
        case TOK_RWORD_BREAK: {
            PUTBACK(tok, lex);
            auto ret = Parse_Stmt(lex);
            if( LOOK_AHEAD(lex) == TOK_EOF ) {
            }
            else if( GET_TOK(tok, lex) != TOK_SEMICOLON ) {
                CHECK_TOK(tok, TOK_BRACE_CLOSE);
                PUTBACK(tok, lex);
            }
            else {
                // return/continue/break don't need silencing
            }
            return ret;
            }

        case TOK_IDENT:
            if( lex.lookahead(0) == TOK_EXCLAM )
            {
                // If a braced macro invocation is the first part of a statement, don't expect a semicolon
                if( lex.lookahead(1) == TOK_BRACE_OPEN || (lex.lookahead(1) == TOK_IDENT && lex.lookahead(2) == TOK_BRACE_OPEN) ) {
                    lex.getToken();
                    return Parse_ExprMacro(lex, tok.str());
                }
            }
        // Fall through to the statement code
        default:
            PUTBACK(tok, lex);
            return Parse_ExprBlockLine_Stmt(lex, *add_silence);
        }
    }
}

ExprNodeP Parse_ExprBlockLine_Stmt(TokenStream& lex, bool& has_semicolon)
{
    Token tok;
    auto ret = Parse_Stmt(lex);
    // If this expression statement wasn't followed by a semicolon, then it's yielding its value out of the block.
    // - I.e. The block should be ending
    if( GET_TOK(tok, lex) != TOK_SEMICOLON ) {
        // - Allow TOK_EOF for macro expansion.
        if( tok.type() == TOK_EOF )
            ;
        else
            CHECK_TOK(tok, TOK_BRACE_CLOSE);
        PUTBACK(tok, lex);
    }
    else {
        has_semicolon = true;
    }
    return ret;
}

/// While loop (either as a statement, or as part of an expression)
ExprNodeP Parse_WhileStmt(TokenStream& lex, ::std::string lifetime)
{
    Token   tok;

    if( GET_TOK(tok, lex) == TOK_RWORD_LET ) {
        auto pat = Parse_Pattern(lex, true);    // Refutable pattern
        GET_CHECK_TOK(tok, lex, TOK_EQUAL);
        ExprNodeP val;
        {
            SET_PARSE_FLAG(lex, disallow_struct_literal);
            val = Parse_Expr0(lex);
        }
        return NEWNODE( AST::ExprNode_Loop, lifetime, AST::ExprNode_Loop::WHILELET,
            ::std::move(pat), ::std::move(val), Parse_ExprBlockNode(lex) );
    }
    else {
        PUTBACK(tok, lex);
        ExprNodeP cnd;
        {
            SET_PARSE_FLAG(lex, disallow_struct_literal);
            cnd = Parse_Expr1(lex);
        }
        return NEWNODE( AST::ExprNode_Loop, lifetime, ::std::move(cnd), Parse_ExprBlockNode(lex) );
    }
}
/// For loop (either as a statement, or as part of an expression)
ExprNodeP Parse_ForStmt(TokenStream& lex, ::std::string lifetime)
{
    Token   tok;

    // Irrefutable pattern
    AST::Pattern    pat = Parse_Pattern(lex, false);
    GET_CHECK_TOK(tok, lex, TOK_RWORD_IN);
    ExprNodeP val;
    {
        SET_PARSE_FLAG(lex, disallow_struct_literal);
        val = Parse_Expr0(lex);
    }
    return NEWNODE( AST::ExprNode_Loop, lifetime, AST::ExprNode_Loop::FOR,
            ::std::move(pat), ::std::move(val), Parse_ExprBlockNode(lex) );
}
/// Parse an 'if' statement
// Note: TOK_RWORD_IF has already been eaten
ExprNodeP Parse_IfStmt(TokenStream& lex)
{
    TRACE_FUNCTION;

    Token   tok;
    ExprNodeP cond;
    AST::Pattern    pat;
    bool if_let = false;

    {
        SET_PARSE_FLAG(lex, disallow_struct_literal);
        if( GET_TOK(tok, lex) == TOK_RWORD_LET ) {
            if_let = true;
            // Refutable pattern
            pat = Parse_Pattern(lex, true);
            GET_CHECK_TOK(tok, lex, TOK_EQUAL);
            cond = Parse_Expr0(lex);
        }
        else {
            PUTBACK(tok, lex);
            cond = Parse_Expr0(lex);
        }
    }

    // Contents
    ExprNodeP code = Parse_ExprBlockNode(lex);

    // Handle else:
    ExprNodeP altcode;
    if( GET_TOK(tok, lex) == TOK_RWORD_ELSE )
    {
        // Recurse for 'else if'
        if( GET_TOK(tok, lex) == TOK_RWORD_IF ) {
            altcode = Parse_IfStmt(lex);
        }
        // - or get block
        else {
            PUTBACK(tok, lex);
            altcode = Parse_ExprBlockNode(lex);
        }
    }
    // - or nothing
    else {
        PUTBACK(tok, lex);
    }

    if( if_let )
        return NEWNODE( AST::ExprNode_IfLet, ::std::move(pat), ::std::move(cond), ::std::move(code), ::std::move(altcode) );
    else
        return NEWNODE( AST::ExprNode_If, ::std::move(cond), ::std::move(code), ::std::move(altcode) );
}
/// "match" block
ExprNodeP Parse_Expr_Match(TokenStream& lex)
{
    TRACE_FUNCTION;
    Token tok;

    CLEAR_PARSE_FLAG(lex, disallow_struct_literal);
    // 1. Get expression
    ExprNodeP   switch_val;
    {
        SET_PARSE_FLAG(lex, disallow_struct_literal);
        switch_val = Parse_Expr1(lex);
    }
    //ASSERT(lex, !CHECK_PARSE_FLAG(lex, disallow_struct_literal) );
    GET_CHECK_TOK(tok, lex, TOK_BRACE_OPEN);

    ::std::vector< AST::ExprNode_Match_Arm >    arms;
    do {
        if( GET_TOK(tok, lex) == TOK_BRACE_CLOSE )
            break;
        PUTBACK(tok, lex);
        AST::ExprNode_Match_Arm    arm;

        arm.m_attrs = Parse_ItemAttrs(lex);

        do {
            // Refutable pattern
            arm.m_patterns.push_back( Parse_Pattern(lex, true) );
        } while( GET_TOK(tok, lex) == TOK_PIPE );

        if( tok.type() == TOK_RWORD_IF )
        {
            arm.m_cond = Parse_Expr1(lex);
            GET_TOK(tok, lex);
        }
        CHECK_TOK(tok, TOK_FATARROW);

        arm.m_code = Parse_Stmt(lex);

        arms.push_back( ::std::move(arm) );

        if( GET_TOK(tok, lex) == TOK_COMMA )
            continue;
        PUTBACK(tok, lex);

    } while( 1 );
    CHECK_TOK(tok, TOK_BRACE_CLOSE);

    return NEWNODE( AST::ExprNode_Match, ::std::move(switch_val), ::std::move(arms) );
}

/// Parses the 'stmt' fragment specifier
/// - Flow control
/// - Expressions
ExprNodeP Parse_Stmt(TokenStream& lex)
{
    TRACE_FUNCTION;
    Token   tok;

    switch(GET_TOK(tok, lex))
    {
    case TOK_INTERPOLATED_STMT:
        return tok.take_frag_node();
    // Duplicated here for the :stmt pattern fragment.
    case TOK_RWORD_LET:
        return Parse_Stmt_Let(lex);
    case TOK_RWORD_RETURN: {
        ExprNodeP   val;
        switch(LOOK_AHEAD(lex))
        {
        case TOK_EOF:
        case TOK_SEMICOLON:
        case TOK_COMMA:
        case TOK_BRACE_CLOSE:
        case TOK_PAREN_CLOSE:
        case TOK_SQUARE_CLOSE:
            break;
        default:
            val = Parse_Expr0(lex);
            break;
        }
        return NEWNODE( AST::ExprNode_Flow, AST::ExprNode_Flow::RETURN, "", ::std::move(val) );
        }
    case TOK_RWORD_CONTINUE:
    case TOK_RWORD_BREAK:
        {
        AST::ExprNode_Flow::Type    type;
        switch(tok.type())
        {
        case TOK_RWORD_CONTINUE: type = AST::ExprNode_Flow::CONTINUE; break;
        case TOK_RWORD_BREAK:    type = AST::ExprNode_Flow::BREAK;    break;
        default:    throw ParseError::BugCheck(/*lex,*/ "continue/break");
        }
        ::std::string   lifetime;
        if( GET_TOK(tok, lex) == TOK_LIFETIME )
        {
            lifetime = tok.str();
            GET_TOK(tok, lex);
        }
        ExprNodeP   val;
        switch(tok.type())
        {
        case TOK_EOF:
        case TOK_SEMICOLON:
        case TOK_COMMA:
        case TOK_BRACE_OPEN:
        case TOK_BRACE_CLOSE:
        case TOK_PAREN_CLOSE:
        case TOK_SQUARE_CLOSE:
            PUTBACK(tok, lex);
            break;
        default:
            PUTBACK(tok, lex);
            val = Parse_Expr1(lex);
            break;
        }
        return NEWNODE( AST::ExprNode_Flow, type, lifetime, ::std::move(val) );
        }
    case TOK_BRACE_OPEN:
        PUTBACK(tok, lex);
        return Parse_ExprBlockNode(lex);
    default:
        PUTBACK(tok, lex);
        return Parse_Expr0(lex);
    }
}

ExprNodeP Parse_Stmt_Let(TokenStream& lex)
{
    Token   tok;
    AST::Pattern pat = Parse_Pattern(lex, false);   // irrefutable
    TypeRef type { lex.point_span() };
    if( GET_TOK(tok, lex) == TOK_COLON ) {
        type = Parse_Type(lex);
        GET_TOK(tok, lex);
    }
    ExprNodeP val;
    if( tok.type() == TOK_EQUAL ) {
        val = Parse_Expr0(lex);
    }
    else {
        PUTBACK(tok, lex);
    }
    return NEWNODE( AST::ExprNode_LetBinding, ::std::move(pat), mv$(type), ::std::move(val) );
}

::std::vector<ExprNodeP> Parse_ParenList(TokenStream& lex)
{
    TRACE_FUNCTION;
    Token   tok;

    CLEAR_PARSE_FLAG(lex, disallow_struct_literal);

    ::std::vector<ExprNodeP> rv;
    GET_CHECK_TOK(tok, lex, TOK_PAREN_OPEN);
    if( GET_TOK(tok, lex) != TOK_PAREN_CLOSE )
    {
        PUTBACK(tok, lex);
        do {
            if( LOOK_AHEAD(lex) == TOK_PAREN_CLOSE ) {
                GET_TOK(tok, lex);
                break;
            }
            rv.push_back( Parse_Expr0(lex) );
        } while( GET_TOK(tok, lex) == TOK_COMMA );
        CHECK_TOK(tok, TOK_PAREN_CLOSE);
    }
    return rv;
}

// 0: Assign
ExprNodeP Parse_Expr0(TokenStream& lex)
{
    //TRACE_FUNCTION;
    Token tok;

    auto expr_attrs = Parse_ItemAttrs(lex);

    ExprNodeP rv = Parse_Expr1(lex);
    auto op = AST::ExprNode_Assign::NONE;
    switch( GET_TOK(tok, lex) )
    {
    case TOK_PLUS_EQUAL:
        op = AST::ExprNode_Assign::ADD; if(0)
    case TOK_DASH_EQUAL:
        op = AST::ExprNode_Assign::SUB; if(0)
    case TOK_STAR_EQUAL:
        op = AST::ExprNode_Assign::MUL; if(0)
    case TOK_SLASH_EQUAL:
        op = AST::ExprNode_Assign::DIV; if(0)
    case TOK_PERCENT_EQUAL:
        op = AST::ExprNode_Assign::MOD; if(0)

    case TOK_AMP_EQUAL:
        op = AST::ExprNode_Assign::AND; if(0)
    case TOK_PIPE_EQUAL:
        op = AST::ExprNode_Assign::OR ; if(0)
    case TOK_CARET_EQUAL:
        op = AST::ExprNode_Assign::XOR; if(0)

    case TOK_DOUBLE_GT_EQUAL:
        op = AST::ExprNode_Assign::SHR; if(0)
    case TOK_DOUBLE_LT_EQUAL:
        op = AST::ExprNode_Assign::SHL; if(0)

    case TOK_EQUAL:
        op = AST::ExprNode_Assign::NONE;
        rv = NEWNODE( AST::ExprNode_Assign, op, ::std::move(rv), Parse_Expr0(lex) );
        rv->set_attrs(mv$(expr_attrs));
        return rv;

    default:
        PUTBACK(tok, lex);
        rv->set_attrs(mv$(expr_attrs));
        return rv;
    }
}


#define LEFTASSOC(cur, _next, cases) \
ExprNodeP _next(TokenStream& lex); \
ExprNodeP cur(TokenStream& lex) \
{ \
    ExprNodeP (*next)(TokenStream&) = _next;\
    ExprNodeP rv = next(lex); \
    while(true) \
    { \
        Token   tok; \
        switch((tok = lex.getToken()).type()) \
        { \
        cases \
        default: \
            /*::std::cout << "<<" << #cur << ::std::endl; */\
            PUTBACK(tok, lex); \
            return rv; \
        } \
    } \
}
bool Parse_IsTokValue(eTokenType tok_type)
{
    switch( tok_type )
    {
    case TOK_DOUBLE_COLON:
    case TOK_IDENT:
    case TOK_INTEGER:
    case TOK_FLOAT:
    case TOK_STRING:
    case TOK_RWORD_TRUE:
    case TOK_RWORD_FALSE:
    case TOK_RWORD_SELF:
    case TOK_RWORD_SUPER:
    case TOK_RWORD_BOX:
    case TOK_RWORD_IN:
    case TOK_PAREN_OPEN:
    case TOK_SQUARE_OPEN:

    case TOK_INTERPOLATED_PATH:
    case TOK_INTERPOLATED_EXPR:

    case TOK_PIPE:
    case TOK_EXCLAM:
    case TOK_DASH:
    case TOK_STAR:
    case TOK_AMP:
        return true;
    default:
        return false;
    }
}
ExprNodeP Parse_Expr1_1(TokenStream& lex);
ExprNodeP Parse_Expr1(TokenStream& lex)
{
    Token   tok;
    ExprNodeP (*next)(TokenStream&) = Parse_Expr1_1;

    auto dest = next(lex);
    if( lex.lookahead(0) == TOK_THINARROW_LEFT )
    {
        GET_TOK(tok, lex);
        auto val = Parse_Expr1(lex);
        return NEWNODE( AST::ExprNode_BinOp, AST::ExprNode_BinOp::PLACE_IN, mv$(dest), mv$(val));
    }
    else
    {
        return dest;
    }
}
ExprNodeP Parse_Expr1_2(TokenStream& lex);
// Very evil handling for '..'
ExprNodeP Parse_Expr1_1(TokenStream& lex)
{
    Token   tok;
    ExprNodeP (*next)(TokenStream&) = Parse_Expr1_2;
    ExprNodeP   left, right;

    // Inclusive range to a value
    if( GET_TOK(tok, lex) == TOK_TRIPLE_DOT ) {
        right = next(lex);
        return NEWNODE( AST::ExprNode_BinOp, AST::ExprNode_BinOp::RANGE_INC, nullptr, mv$(right) );
    }
    else {
        PUTBACK(tok, lex);
    }

    // Exclusive ranges
    // - If NOT `.. <VAL>`, parse a leading value
    if( GET_TOK(tok, lex) != TOK_DOUBLE_DOT )
    {
        PUTBACK(tok, lex);

        left = next(lex);

        // - If NOT `<VAL> ..`, return the value
        if( GET_TOK(tok, lex) != TOK_DOUBLE_DOT )
        {
            PUTBACK(tok, lex);
            return ::std::move(left);
        }
    }
    assert( tok.type() == TOK_DOUBLE_DOT );
    // If the next token is part of a value, parse that value
    if( Parse_IsTokValue( LOOK_AHEAD(lex) ) )
    {
        right = next(lex);
    }
    else
    {
        // Otherwise, leave `right` as nullptr
    }

    return NEWNODE( AST::ExprNode_BinOp, AST::ExprNode_BinOp::RANGE, ::std::move(left), ::std::move(right) );
}
LEFTASSOC(Parse_Expr1_2, Parse_Expr1_5,
    case TOK_TRIPLE_DOT:
        rv = NEWNODE( AST::ExprNode_BinOp, AST::ExprNode_BinOp::RANGE_INC, mv$(rv), next(lex) );
        break;
)
// 1: Bool OR
LEFTASSOC(Parse_Expr1_5, Parse_Expr2,
    case TOK_DOUBLE_PIPE:
        rv = NEWNODE( AST::ExprNode_BinOp, AST::ExprNode_BinOp::BOOLOR, ::std::move(rv), next(lex));
        break;
)
// 2: Bool AND
LEFTASSOC(Parse_Expr2, Parse_Expr3,
    case TOK_DOUBLE_AMP:
        rv = NEWNODE( AST::ExprNode_BinOp, AST::ExprNode_BinOp::BOOLAND, ::std::move(rv), next(lex));
        break;
)
// 3: (In)Equality
LEFTASSOC(Parse_Expr3, Parse_Expr4,
    case TOK_DOUBLE_EQUAL:
        rv = NEWNODE( AST::ExprNode_BinOp, AST::ExprNode_BinOp::CMPEQU, ::std::move(rv), next(lex));
        break;
    case TOK_EXCLAM_EQUAL:
        rv = NEWNODE( AST::ExprNode_BinOp, AST::ExprNode_BinOp::CMPNEQU, ::std::move(rv), next(lex));
        break;
)
// 4: Comparisons
LEFTASSOC(Parse_Expr4, Parse_Expr5,
    case TOK_LT:
        rv = NEWNODE( AST::ExprNode_BinOp, AST::ExprNode_BinOp::CMPLT, ::std::move(rv), next(lex));
        break;
    case TOK_GT:
        rv = NEWNODE( AST::ExprNode_BinOp, AST::ExprNode_BinOp::CMPGT, ::std::move(rv), next(lex));
        break;
    case TOK_LTE:
        rv = NEWNODE( AST::ExprNode_BinOp, AST::ExprNode_BinOp::CMPLTE, ::std::move(rv), next(lex));
        break;
    case TOK_GTE:
        rv = NEWNODE( AST::ExprNode_BinOp, AST::ExprNode_BinOp::CMPGTE, ::std::move(rv), next(lex));
        break;
)
// 5: Bit OR
LEFTASSOC(Parse_Expr5, Parse_Expr6,
    case TOK_PIPE:
        rv = NEWNODE( AST::ExprNode_BinOp, AST::ExprNode_BinOp::BITOR, ::std::move(rv), next(lex));
        break;
)
// 6: Bit XOR
LEFTASSOC(Parse_Expr6, Parse_Expr7,
    case TOK_CARET:
        rv = NEWNODE( AST::ExprNode_BinOp, AST::ExprNode_BinOp::BITXOR, ::std::move(rv), next(lex));
        break;
)
// 7: Bit AND
LEFTASSOC(Parse_Expr7, Parse_Expr8,
    case TOK_AMP:
        rv = NEWNODE( AST::ExprNode_BinOp, AST::ExprNode_BinOp::BITAND, ::std::move(rv), next(lex));
        break;
)
// 8: Bit Shifts
LEFTASSOC(Parse_Expr8, Parse_Expr9,
    case TOK_DOUBLE_LT:
        rv = NEWNODE( AST::ExprNode_BinOp, AST::ExprNode_BinOp::SHL, ::std::move(rv), next(lex));
        break;
    case TOK_DOUBLE_GT:
        rv = NEWNODE( AST::ExprNode_BinOp, AST::ExprNode_BinOp::SHR, ::std::move(rv), next(lex));
        break;
)
// 9: Add / Subtract
LEFTASSOC(Parse_Expr9, Parse_Expr10,
    case TOK_PLUS:
        rv = NEWNODE( AST::ExprNode_BinOp, AST::ExprNode_BinOp::ADD, ::std::move(rv), next(lex));
        break;
    case TOK_DASH:
        rv = NEWNODE( AST::ExprNode_BinOp, AST::ExprNode_BinOp::SUB, ::std::move(rv), next(lex));
        break;
)
// 10: Times / Divide / Modulo
LEFTASSOC(Parse_Expr10, Parse_Expr11,
    case TOK_STAR:
        rv = NEWNODE( AST::ExprNode_BinOp, AST::ExprNode_BinOp::MULTIPLY, ::std::move(rv), next(lex));
        break;
    case TOK_SLASH:
        rv = NEWNODE( AST::ExprNode_BinOp, AST::ExprNode_BinOp::DIVIDE, ::std::move(rv), next(lex));
        break;
    case TOK_PERCENT:
        rv = NEWNODE( AST::ExprNode_BinOp, AST::ExprNode_BinOp::MODULO, ::std::move(rv), next(lex));
        break;
)
// 11: Cast
LEFTASSOC(Parse_Expr11, Parse_Expr12,
    case TOK_RWORD_AS:
        rv = NEWNODE( AST::ExprNode_Cast, ::std::move(rv), Parse_Type(lex, false) );
        break;
)
// 12: Type Ascription
ExprNodeP Parse_Expr13(TokenStream& lex);
ExprNodeP Parse_Expr12(TokenStream& lex)
{
    Token   tok;
    auto rv = Parse_Expr13(lex);
    if(GET_TOK(tok, lex) == TOK_COLON)
    {
        rv = NEWNODE( AST::ExprNode_TypeAnnotation, mv$(rv), Parse_Type(lex) );
    }
    else
    {
        PUTBACK(tok, lex);
    }
    return rv;
}
// 13: Unaries
ExprNodeP Parse_ExprFC(TokenStream& lex);
ExprNodeP Parse_Expr13(TokenStream& lex)
{
    Token   tok;
    switch(GET_TOK(tok, lex))
    {
    case TOK_DASH:
        return NEWNODE( AST::ExprNode_UniOp, AST::ExprNode_UniOp::NEGATE, Parse_Expr12(lex) );
    case TOK_EXCLAM:
        return NEWNODE( AST::ExprNode_UniOp, AST::ExprNode_UniOp::INVERT, Parse_Expr12(lex) );
    case TOK_STAR:
        return NEWNODE( AST::ExprNode_Deref, Parse_Expr12(lex) );
    case TOK_RWORD_BOX:
        return NEWNODE( AST::ExprNode_UniOp, AST::ExprNode_UniOp::BOX, Parse_Expr12(lex) );
    case TOK_RWORD_IN: {
        ExprNodeP   dest;
        {
            SET_PARSE_FLAG(lex, disallow_struct_literal);
            dest = Parse_Expr1(lex);
        }
        auto val = Parse_ExprBlockNode(lex);
        return NEWNODE( AST::ExprNode_BinOp, AST::ExprNode_BinOp::PLACE_IN, mv$(dest), mv$(val));
        }
    case TOK_DOUBLE_AMP:
        // HACK: Split && into & &
        lex.putback( Token(TOK_AMP) );
    case TOK_AMP:
        if( GET_TOK(tok, lex) == TOK_RWORD_MUT )
            return NEWNODE( AST::ExprNode_UniOp, AST::ExprNode_UniOp::REFMUT, Parse_Expr12(lex) );
        else {
            PUTBACK(tok, lex);
            return NEWNODE( AST::ExprNode_UniOp, AST::ExprNode_UniOp::REF, Parse_Expr12(lex) );
        }
    default:
        PUTBACK(tok, lex);
        return Parse_ExprFC(lex);
    }
}

ExprNodeP Parse_ExprVal(TokenStream& lex);
ExprNodeP Parse_ExprFC(TokenStream& lex)
{
    ExprNodeP   val = Parse_ExprVal(lex);
    while(true)
    {
        Token   tok;
        switch(GET_TOK(tok, lex))
        {
        case TOK_QMARK:
            val = NEWNODE( AST::ExprNode_UniOp, AST::ExprNode_UniOp::QMARK, mv$(val) );
            break;

        case TOK_PAREN_OPEN:
            // Expression method call
            PUTBACK(tok, lex);
            val = NEWNODE( AST::ExprNode_CallObject, ::std::move(val), Parse_ParenList(lex) );
            break;
        case TOK_SQUARE_OPEN:
            val = NEWNODE( AST::ExprNode_Index, ::std::move(val), Parse_Expr0(lex) );
            GET_CHECK_TOK(tok, lex, TOK_SQUARE_CLOSE);
            break;
        case TOK_DOT:
            // Field access / method call / tuple index
            switch(GET_TOK(tok, lex))
            {
            case TOK_IDENT: {
                AST::PathNode   path( mv$(tok.str()) , {});
                switch( GET_TOK(tok, lex) )
                {
                case TOK_PAREN_OPEN:
                    PUTBACK(tok, lex);
                    val = NEWNODE( AST::ExprNode_CallMethod, ::std::move(val), ::std::move(path), Parse_ParenList(lex) );
                    break;
                case TOK_DOUBLE_COLON:
                    GET_CHECK_TOK(tok, lex, TOK_LT);
                    path.args() = Parse_Path_GenericList(lex);
                    val = NEWNODE( AST::ExprNode_CallMethod, ::std::move(val), ::std::move(path), Parse_ParenList(lex) );
                    break;
                default:
                    val = NEWNODE( AST::ExprNode_Field, ::std::move(val), ::std::string(path.name()) );
                    PUTBACK(tok, lex);
                    break;
                }
                break; }
            case TOK_INTEGER:
                val = NEWNODE( AST::ExprNode_Field, ::std::move(val), FMT(tok.intval()) );
                break;
            default:
                throw ParseError::Unexpected(lex, mv$(tok));
            }
            break;
        default:
            PUTBACK(tok, lex);
            return val;
        }
    }
}

ExprNodeP Parse_ExprVal_StructLiteral(TokenStream& lex, AST::Path path)
{
    TRACE_FUNCTION;
    Token   tok;

    // #![feature(relaxed_adts)]
    if( LOOK_AHEAD(lex) == TOK_INTEGER )
    {
        ::std::map<unsigned int, ExprNodeP> nodes;
        while( GET_TOK(tok, lex) == TOK_INTEGER )
        {
            unsigned int ofs = static_cast<unsigned int>(tok.intval());
            GET_CHECK_TOK(tok, lex, TOK_COLON);
            ExprNodeP   val = Parse_Stmt(lex);
            if( ! nodes.insert( ::std::make_pair(ofs, mv$(val)) ).second ) {
                ERROR(lex.point_span(), E0000, "Duplicate index");
            }

            if( GET_TOK(tok,lex) == TOK_BRACE_CLOSE )
                break;
            CHECK_TOK(tok, TOK_COMMA);
        }
        CHECK_TOK(tok, TOK_BRACE_CLOSE);

        ::std::vector<ExprNodeP>    items;
        unsigned int i = 0;
        for(auto& p : nodes)
        {
            if( p.first != i ) {
                ERROR(lex.point_span(), E0000, "Missing index " << i);
            }
            items.push_back( mv$(p.second) );
            i ++;
        }

        return NEWNODE( AST::ExprNode_CallPath, mv$(path), mv$(items) );
    }

    // Braced structure literal
    // - A series of 0 or more pairs of <ident>: <expr>,
    // - '..' <expr>
    ::AST::ExprNode_StructLiteral::t_values items;
    while( GET_TOK(tok, lex) == TOK_IDENT || tok.type() == TOK_HASH )
    {
        ::AST::AttributeList attrs;    // Note: Parse_ItemAttrs uses lookahead, so can't use it here.
        if( tok.type() == TOK_HASH )
        {
            PUTBACK(tok, lex);
            attrs = Parse_ItemAttrs(lex);
            GET_TOK(tok, lex);
        }
        CHECK_TOK(tok, TOK_IDENT);
        auto h = lex.getHygiene();
        auto name = mv$(tok.str());

        ExprNodeP   val;
        if( lex.lookahead(0) != TOK_COLON )
        {
            val = NEWNODE( AST::ExprNode_NamedValue, ::AST::Path(::AST::Path::TagRelative{}, h, { ::AST::PathNode(name) }) );
        }
        else
        {
            GET_CHECK_TOK(tok, lex, TOK_COLON);
            val = Parse_Stmt(lex);
        }
        items.push_back(::AST::ExprNode_StructLiteral::Ent { mv$(attrs), mv$(name), mv$(val) });

        if( GET_TOK(tok,lex) == TOK_BRACE_CLOSE )
            break;
        CHECK_TOK(tok, TOK_COMMA);
    }
    ExprNodeP    base_val;
    if( tok.type() == TOK_DOUBLE_DOT )
    {
        // default
        base_val = Parse_Expr0(lex);
        GET_TOK(tok, lex);
    }
    CHECK_TOK(tok, TOK_BRACE_CLOSE);

    return NEWNODE( AST::ExprNode_StructLiteral, path, ::std::move(base_val), ::std::move(items) );
}

ExprNodeP Parse_ExprVal_Closure(TokenStream& lex, bool is_move)
{
    TRACE_FUNCTION;
    Token   tok;

    ::std::vector< ::std::pair<AST::Pattern, TypeRef> > args;

    while( GET_TOK(tok, lex) != TOK_PIPE )
    {
        PUTBACK(tok, lex);
        // Irrefutable pattern
        AST::Pattern    pat = Parse_Pattern(lex, false);

        TypeRef type { lex.point_span() };
        if( GET_TOK(tok, lex) == TOK_COLON )
            type = Parse_Type(lex);
        else
            PUTBACK(tok, lex);

        args.push_back( ::std::make_pair( ::std::move(pat), ::std::move(type) ) );

        if( GET_TOK(tok, lex) != TOK_COMMA )
            break;
    }
    CHECK_TOK(tok, TOK_PIPE);

    auto rt = TypeRef(lex.point_span());
    if( GET_TOK(tok, lex) == TOK_THINARROW ) {

        if( GET_TOK(tok, lex) == TOK_EXCLAM ) {
            rt = TypeRef(TypeRef::TagInvalid(), Span(tok.get_pos()));
        }
        else {
            PUTBACK(tok, lex);
            rt = Parse_Type(lex);
        }
    }
    else
        PUTBACK(tok, lex);

    auto code = Parse_Expr0(lex);

    return NEWNODE( AST::ExprNode_Closure, ::std::move(args), ::std::move(rt), ::std::move(code), is_move );
}

ExprNodeP Parse_ExprVal(TokenStream& lex)
{
    TRACE_FUNCTION;

    Token   tok;
    AST::Path   path;
    switch( GET_TOK(tok, lex) )
    {
    case TOK_BRACE_OPEN:
        PUTBACK(tok, lex);
        return Parse_ExprBlockNode(lex);

    case TOK_INTERPOLATED_EXPR:
    case TOK_INTERPOLATED_BLOCK:
        return tok.take_frag_node();


    // Return/break/continue/... also parsed here (but recurses back up to actually handle them)
    case TOK_RWORD_RETURN:
    case TOK_RWORD_CONTINUE:
    case TOK_RWORD_BREAK:
        PUTBACK(tok, lex);
        return Parse_Stmt(lex);

    case TOK_RWORD_LOOP:
        return NEWNODE( AST::ExprNode_Loop, "", Parse_ExprBlockNode(lex) );
    case TOK_RWORD_WHILE:
        return Parse_WhileStmt(lex, "");
    case TOK_RWORD_FOR:
        return Parse_ForStmt(lex, "");
    case TOK_RWORD_MATCH:
        return Parse_Expr_Match(lex);
    case TOK_RWORD_IF:
        return Parse_IfStmt(lex);
    case TOK_RWORD_UNSAFE:
        return Parse_ExprBlockNode(lex, true);

    // UFCS
    case TOK_DOUBLE_LT:
    case TOK_LT:
        PUTBACK(tok, lex);
        path = Parse_Path(lex, PATH_GENERIC_EXPR);
        // Skip down to method
        if(0)
    case TOK_RWORD_SELF:
        {
            if( LOOK_AHEAD(lex) != TOK_DOUBLE_COLON ) {
                return NEWNODE( AST::ExprNode_NamedValue, AST::Path(AST::Path::TagLocal(), "self") );
            }
            else
            {
                PUTBACK(tok, lex);
                path = Parse_Path(lex, PATH_GENERIC_EXPR);
            }
        }
        if(0)
    case TOK_RWORD_SUPER:
        {
            PUTBACK(tok, lex);
            path = Parse_Path(lex, PATH_GENERIC_EXPR);
        }
        if(0)
    case TOK_IDENT:
        // Get path
        {
            PUTBACK(tok, lex);
            path = Parse_Path(lex, false, PATH_GENERIC_EXPR);
        }
        if(0)
    case TOK_INTERPOLATED_PATH:
        {
            path = mv$(tok.frag_path());
        }
        if(0)
    case TOK_DOUBLE_COLON:
        path = Parse_Path(lex, true, PATH_GENERIC_EXPR);
        DEBUG("path = " << path << ", lookahead=" << Token::typestr(lex.lookahead(0)));
        // SKIP TARGET
        switch( GET_TOK(tok, lex) )
        {
        case TOK_EXCLAM:
            return Parse_ExprMacro(lex, mv$(path));
        case TOK_PAREN_OPEN:
            // Function call
            PUTBACK(tok, lex);
            return NEWNODE( AST::ExprNode_CallPath, ::std::move(path), Parse_ParenList(lex) );
        case TOK_BRACE_OPEN:
            if( !CHECK_PARSE_FLAG(lex, disallow_struct_literal) )
                return Parse_ExprVal_StructLiteral(lex, ::std::move(path));
            else
                DEBUG("Not parsing struct literal");
        default:
            // Value
            PUTBACK(tok, lex);
            return NEWNODE( AST::ExprNode_NamedValue, ::std::move(path) );
        }
    case TOK_RWORD_MOVE:
        GET_TOK(tok, lex);
        if(tok.type() == TOK_PIPE)
            return Parse_ExprVal_Closure(lex, true);
        else if(tok.type() == TOK_DOUBLE_PIPE) {
            lex.putback(Token(TOK_PIPE));
            return Parse_ExprVal_Closure(lex, true);
        }
        else {
            CHECK_TOK(tok, TOK_PIPE);
        }
    case TOK_DOUBLE_PIPE:
        lex.putback(Token(TOK_PIPE));
    case TOK_PIPE:
        return Parse_ExprVal_Closure(lex, false);
    case TOK_INTEGER:
        return NEWNODE( AST::ExprNode_Integer, tok.intval(), tok.datatype() );
    case TOK_FLOAT:
        return NEWNODE( AST::ExprNode_Float, tok.floatval(), tok.datatype() );
    case TOK_STRING:
        return NEWNODE( AST::ExprNode_String, tok.str() );
    case TOK_BYTESTRING:
        return NEWNODE( AST::ExprNode_ByteString, tok.str() );
    case TOK_RWORD_TRUE:
        return NEWNODE( AST::ExprNode_Bool, true );
    case TOK_RWORD_FALSE:
        return NEWNODE( AST::ExprNode_Bool, false );
    case TOK_PAREN_OPEN:
        if( GET_TOK(tok, lex) == TOK_PAREN_CLOSE )
        {
            DEBUG("Unit");
            return NEWNODE( AST::ExprNode_Tuple, ::std::vector<ExprNodeP>() );
        }
        else
        {
            CLEAR_PARSE_FLAG(lex, disallow_struct_literal);
            PUTBACK(tok, lex);

            ExprNodeP rv = Parse_Expr0(lex);
            if( GET_TOK(tok, lex) == TOK_COMMA ) {
                ::std::vector<ExprNodeP> ents;
                ents.push_back( ::std::move(rv) );
                do {
                    if( GET_TOK(tok, lex) == TOK_PAREN_CLOSE )
                        break;
                    PUTBACK(tok, lex);
                    ents.push_back( Parse_Expr0(lex) );
                } while( GET_TOK(tok, lex) == TOK_COMMA );
                rv = NEWNODE( AST::ExprNode_Tuple, ::std::move(ents) );
            }
            CHECK_TOK(tok, TOK_PAREN_CLOSE);
            return rv;
        }
    case TOK_SQUARE_OPEN:
        if( GET_TOK(tok, lex) == TOK_SQUARE_CLOSE )
        {
            // Empty literal
            return NEWNODE( AST::ExprNode_Array, ::std::vector<ExprNodeP>() );
        }
        else
        {
            PUTBACK(tok, lex);
            auto first = Parse_Expr0(lex);
            if( GET_TOK(tok, lex) == TOK_SEMICOLON )
            {
                // Repetiion
                auto count = Parse_Expr0(lex);
                GET_CHECK_TOK(tok, lex, TOK_SQUARE_CLOSE);
                return NEWNODE( AST::ExprNode_Array, ::std::move(first), ::std::move(count) );
            }
            else
            {
                ::std::vector<ExprNodeP>    items;
                items.push_back( ::std::move(first) );
                while( tok.type() == TOK_COMMA )
                {
                    if( GET_TOK(tok, lex) == TOK_SQUARE_CLOSE )
                        break;
                    else
                        PUTBACK(tok, lex);
                    items.push_back( Parse_Expr0(lex) );
                    GET_TOK(tok, lex);
                }
                CHECK_TOK(tok, TOK_SQUARE_CLOSE);
                return NEWNODE( AST::ExprNode_Array, ::std::move(items) );
            }
        }
        throw ParseError::BugCheck(lex, "Array literal fell");
    default:
        throw ParseError::Unexpected(lex, tok);
    }
}
ExprNodeP Parse_ExprMacro(TokenStream& lex, AST::Path path)
{
    if( !path.is_trivial() ) {
        TODO(lex.point_span(), "Support path macros - " << path);
    }

    Token   tok;
    ::std::string name = path.m_class.is_Local() ? path.m_class.as_Local().name : path.nodes()[0].name();
    ::std::string ident;
    if( GET_TOK(tok, lex) == TOK_IDENT ) {
        ident = mv$(tok.str());
    }
    else {
        PUTBACK(tok, lex);
    }
    TokenTree tt = Parse_TT(lex, true);
    if( tt.is_token() ) {
        throw ParseError::Unexpected(lex, tt.tok());
    }
    return NEWNODE(AST::ExprNode_Macro, mv$(name), mv$(ident), mv$(tt));
}

// Token Tree Parsing
TokenTree Parse_TT(TokenStream& lex, bool unwrapped)
{
    TokenTree   rv;
    TRACE_FUNCTION_FR("", rv);

    Token tok = lex.getToken();
    eTokenType  closer = TOK_PAREN_CLOSE;
    switch(tok.type())
    {
    case TOK_PAREN_OPEN:
        closer = TOK_PAREN_CLOSE;
        break;
    case TOK_SQUARE_OPEN:
        closer = TOK_SQUARE_CLOSE;
        break;
    case TOK_BRACE_OPEN:
        closer = TOK_BRACE_CLOSE;
        break;

    case TOK_EOF:
    case TOK_NULL:
    case TOK_PAREN_CLOSE:
    case TOK_SQUARE_CLOSE:
    case TOK_BRACE_CLOSE:
        throw ParseError::Unexpected(lex, tok);
    default:
        rv = TokenTree(lex.getHygiene(), mv$(tok) );
        return rv;
    }

    ::std::vector<TokenTree>   items;
    if( !unwrapped )
        items.push_back( TokenTree(lex.getHygiene(), mv$(tok)) );
    while(GET_TOK(tok, lex) != closer && tok.type() != TOK_EOF)
    {
        if( tok.type() == TOK_NULL )
            throw ParseError::Unexpected(lex, tok);
        PUTBACK(tok, lex);
        items.push_back(Parse_TT(lex, false));
    }
    if( !unwrapped )
        items.push_back( TokenTree(lex.getHygiene(), mv$(tok)) );
    rv = TokenTree(lex.getHygiene(), mv$(items));
    return rv;
}
