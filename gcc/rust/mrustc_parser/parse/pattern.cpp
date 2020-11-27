/*
 * MRustC - Rust Compiler
 * - By John Hodge (Mutabah/thePowersGang)
 *
 * parse/pattern.cpp
 * - Parsing for patterns
 */
#include "common.hpp"
#include "parseerror.hpp"
#include <ast/expr.hpp> // To convert :expr

// NEWNODE is needed for the Value pattern type
typedef ::std::unique_ptr<AST::ExprNode>    ExprNodeP;
#define NEWNODE(type, ...)  ExprNodeP(new type(__VA_ARGS__))
using AST::ExprNode;



::AST::Pattern::TuplePat Parse_PatternTuple(TokenStream& lex, bool is_refutable);
AST::Pattern Parse_PatternReal_Slice(TokenStream& lex, bool is_refutable);
AST::Pattern Parse_PatternReal_Path(TokenStream& lex, ProtoSpan ps, AST::Path path, bool is_refutable);
AST::Pattern Parse_PatternReal(TokenStream& lex, bool is_refutable);
AST::Pattern Parse_PatternStruct(TokenStream& lex, ProtoSpan ps, AST::Path path, bool is_refutable);

AST::Pattern Parse_PatternReal(TokenStream& lex, bool is_refutable);
AST::Pattern Parse_PatternReal1(TokenStream& lex, bool is_refutable);


/// Parse a pattern
///
/// Examples:
/// - `Enum::Variant(a)`
/// - `(1, a)`
/// - `1 ... 2`
/// - `"string"`
/// - `mut x`
/// - `mut x @ 1 ... 2`
AST::Pattern Parse_Pattern(TokenStream& lex, bool is_refutable)
{
    TRACE_FUNCTION;
    auto ps = lex.start_span();

    Token   tok;
    tok = lex.getToken();

    if( tok.type() == TOK_IDENT && lex.lookahead(0) == TOK_EXCLAM )
    {
        lex.getToken();
        return AST::Pattern( AST::Pattern::TagMacro(), lex.end_span(ps), box$(Parse_MacroInvocation(ps, tok.str(), lex)));
    }
    if( tok.type() == TOK_INTERPOLATED_PATTERN )
    {
        return mv$(tok.frag_pattern());
    }

    bool expect_bind = false;
    auto bind_type = AST::PatternBinding::Type::MOVE;
    bool is_mut = false;
    // 1. Mutablity + Reference
    if( tok.type() == TOK_RWORD_REF )
    {
        expect_bind = true;
        tok = lex.getToken();
        if( tok.type() == TOK_RWORD_MUT )
        {
            bind_type = AST::PatternBinding::Type::MUTREF;
            GET_TOK(tok, lex);
        }
        else
        {
            bind_type = AST::PatternBinding::Type::REF;
        }
    }
    else if( tok.type() == TOK_RWORD_MUT )
    {
        is_mut = true;
        expect_bind = true;
        GET_TOK(tok, lex);
    }
    else
    {
        // Fall through
    }

    AST::PatternBinding binding;
    // If a 'ref' or 'mut' annotation was seen, the next name must be a binding name
    if( expect_bind )
    {
        CHECK_TOK(tok, TOK_IDENT);
        auto bind_name = Ident(lex.getHygiene(), mv$(tok.str()));
        // If there's no '@' after it, it's a name binding only (_ pattern)
        if( GET_TOK(tok, lex) != TOK_AT )
        {
            PUTBACK(tok, lex);
            return AST::Pattern(AST::Pattern::TagBind(), lex.end_span(mv$(ps)), mv$(bind_name), bind_type, is_mut);
        }
        binding = AST::PatternBinding( mv$(bind_name), bind_type, is_mut );

        // '@' consumed, move on to next token
        GET_TOK(tok, lex);
    }
    // Otherwise, handle MaybeBind
    else if( tok.type() == TOK_IDENT )
    {
        switch( LOOK_AHEAD(lex) )
        {
        // Known path `ident::`
        case TOK_DOUBLE_COLON:
            break;
        // Known struct `Ident {` or `Ident (`
        case TOK_BRACE_OPEN:
        case TOK_PAREN_OPEN:
            break;
        // Known value `IDENT ...`
        case TOK_TRIPLE_DOT:
            break;
        // Known binding `ident @`
        case TOK_AT:
            binding = AST::PatternBinding( Ident(lex.getHygiene(), mv$(tok.str())), bind_type/*MOVE*/, is_mut/*false*/ );
            GET_TOK(tok, lex);  // '@'
            GET_TOK(tok, lex);  // Match lex.putback() below
            break;
        default: {  // Maybe bind
            Ident   name = Ident(lex.getHygiene(), mv$(tok.str()));
            // if the pattern can be refuted (i.e this could be an enum variant), return MaybeBind
            if( is_refutable ) {
                assert(bind_type == ::AST::PatternBinding::Type::MOVE);
                assert(is_mut == false);
                return AST::Pattern(AST::Pattern::TagMaybeBind(), lex.end_span(mv$(ps)), mv$(name));
            }
            // Otherwise, it IS a binding
            else {
                return AST::Pattern(AST::Pattern::TagBind(), lex.end_span(mv$(ps)), mv$(name), bind_type, is_mut);
            }
            break;}
        }
    }
    else
    {
        // Otherwise, fall through
    }

    PUTBACK(tok, lex);
    auto pat = Parse_PatternReal(lex, is_refutable);
    pat.binding() = mv$(binding);
    return pat;
}

AST::Pattern Parse_PatternReal(TokenStream& lex, bool is_refutable)
{
    Token   tok;
    if( LOOK_AHEAD(lex) == TOK_INTERPOLATED_PATTERN )
    {
        GET_TOK(tok, lex);
        return mv$(tok.frag_pattern());
    }
    auto ps = lex.start_span();
    AST::Pattern    ret = Parse_PatternReal1(lex, is_refutable);
    if( GET_TOK(tok, lex) == TOK_TRIPLE_DOT )
    {
        if( !ret.data().is_Value() )
            throw ParseError::Generic(lex, "Using '...' with a non-value on left");
        auto& ret_v = ret.data().as_Value();

        auto    right_pat = Parse_PatternReal1(lex, is_refutable);
        if( !right_pat.data().is_Value() )
            throw ParseError::Generic(lex, "Using '...' with a non-value on right");
        auto    rightval = mv$( right_pat.data().as_Value().start );
        ret_v.end = mv$(rightval);
        // TODO: use `ps` here?

        return ret;
    }
    else
    {
        PUTBACK(tok, lex);
        return ret;
    }
}
AST::Pattern Parse_PatternReal1(TokenStream& lex, bool is_refutable)
{
    TRACE_FUNCTION;
    auto ps = lex.start_span();

    Token   tok;
    AST::Path   path;

    switch( GET_TOK(tok, lex) )
    {
    case TOK_UNDERSCORE:
        return AST::Pattern( lex.end_span(mv$(ps)), AST::Pattern::Data() );
    //case TOK_DOUBLE_DOT:
    //    return AST::Pattern( AST::Pattern::TagWildcard() );
    case TOK_RWORD_BOX:
        return AST::Pattern( AST::Pattern::TagBox(), lex.end_span(mv$(ps)), Parse_Pattern(lex, is_refutable) );
    case TOK_DOUBLE_AMP:
        lex.putback(TOK_AMP);
    case TOK_AMP: {
        DEBUG("Ref");
        // NOTE: Falls back into "Pattern" not "PatternReal" to handle MaybeBind again
        bool is_mut = false;
        if( GET_TOK(tok, lex) == TOK_RWORD_MUT )
            is_mut = true;
        else
            PUTBACK(tok, lex);
        return AST::Pattern( AST::Pattern::TagReference(), lex.end_span(mv$(ps)), is_mut, Parse_Pattern(lex, is_refutable) );
        }
    case TOK_RWORD_SELF:
    case TOK_RWORD_SUPER:
    case TOK_IDENT:
    case TOK_LT:
    case TOK_DOUBLE_LT:
    case TOK_INTERPOLATED_PATH:
        PUTBACK(tok, lex);
        return Parse_PatternReal_Path( lex, ps, Parse_Path(lex, PATH_GENERIC_EXPR), is_refutable );
    case TOK_DOUBLE_COLON:
        // 2. Paths are enum/struct names
        return Parse_PatternReal_Path( lex, ps, Parse_Path(lex, true, PATH_GENERIC_EXPR), is_refutable );
    case TOK_DASH:
        if(GET_TOK(tok, lex) == TOK_INTEGER)
        {
            auto dt = tok.datatype();
            // TODO: Ensure that the type is ANY or a signed integer
            return AST::Pattern( AST::Pattern::TagValue(), lex.end_span(mv$(ps)), AST::Pattern::Value::make_Integer({dt, -tok.intval()}) );
        }
        else if( tok.type() == TOK_FLOAT )
        {
            return AST::Pattern( AST::Pattern::TagValue(), lex.end_span(mv$(ps)), AST::Pattern::Value::make_Float({tok.datatype(), -tok.floatval()}) );
        }
        else
        {
            throw ParseError::Unexpected(lex, tok, {TOK_INTEGER, TOK_FLOAT});
        }
    case TOK_FLOAT:
        return AST::Pattern( AST::Pattern::TagValue(), lex.end_span(mv$(ps)), AST::Pattern::Value::make_Float({tok.datatype(), tok.floatval()}) );
    case TOK_INTEGER:
        return AST::Pattern( AST::Pattern::TagValue(), lex.end_span(mv$(ps)), AST::Pattern::Value::make_Integer({tok.datatype(), tok.intval()}) );
    case TOK_RWORD_TRUE:
        return AST::Pattern( AST::Pattern::TagValue(), lex.end_span(mv$(ps)), AST::Pattern::Value::make_Integer({CORETYPE_BOOL, 1}) );
    case TOK_RWORD_FALSE:
        return AST::Pattern( AST::Pattern::TagValue(), lex.end_span(mv$(ps)), AST::Pattern::Value::make_Integer({CORETYPE_BOOL, 0}) );
    case TOK_STRING:
        return AST::Pattern( AST::Pattern::TagValue(), lex.end_span(mv$(ps)), AST::Pattern::Value::make_String( mv$(tok.str()) ) );
    case TOK_BYTESTRING:
        return AST::Pattern( AST::Pattern::TagValue(), lex.end_span(mv$(ps)), AST::Pattern::Value::make_ByteString({ mv$(tok.str()) }) );
    case TOK_INTERPOLATED_EXPR: {
        auto e = tok.take_frag_node();
        if( auto* n = dynamic_cast<AST::ExprNode_String*>(e.get()) ) {
            return AST::Pattern( AST::Pattern::TagValue(), lex.end_span(mv$(ps)), AST::Pattern::Value::make_String( mv$(n->m_value) ) );
        }
        //else if( auto* n = dynamic_cast<AST::ExprNode_ByteString*>(e.get()) ) {
        //    return AST::Pattern( AST::Pattern::TagValue(), lex.end_span(mv$(ps)), AST::Pattern::Value::make_ByteString( mv$(n->m_value) ) );
        //}
        else if( auto* n = dynamic_cast<AST::ExprNode_Bool*>(e.get()) ) {
            return AST::Pattern( AST::Pattern::TagValue(), lex.end_span(mv$(ps)), AST::Pattern::Value::make_Integer({CORETYPE_BOOL, n->m_value}) );
        }
        else if( auto* n = dynamic_cast<AST::ExprNode_Integer*>(e.get()) ) {
            return AST::Pattern( AST::Pattern::TagValue(), lex.end_span(mv$(ps)), AST::Pattern::Value::make_Integer({n->m_datatype, n->m_value}) );
        }
        else if( auto* n = dynamic_cast<AST::ExprNode_Float*>(e.get()) ) {
            return AST::Pattern( AST::Pattern::TagValue(), lex.end_span(mv$(ps)), AST::Pattern::Value::make_Float({n->m_datatype, n->m_value}) );
        }
        else {
            TODO(lex.point_span(), "Convert :expr into a pattern value - " << *e);
        }
        } break;

    case TOK_PAREN_OPEN:
        return AST::Pattern( AST::Pattern::TagTuple(), lex.end_span(mv$(ps)), Parse_PatternTuple(lex, is_refutable) );
    case TOK_SQUARE_OPEN:
        return Parse_PatternReal_Slice(lex, is_refutable);
    default:
        throw ParseError::Unexpected(lex, tok);
    }
}
AST::Pattern Parse_PatternReal_Path(TokenStream& lex, ProtoSpan ps, AST::Path path, bool is_refutable)
{
    Token   tok;

    switch( GET_TOK(tok, lex) )
    {
    case TOK_PAREN_OPEN:
        return AST::Pattern( AST::Pattern::TagNamedTuple(), lex.end_span(mv$(ps)), mv$(path), Parse_PatternTuple(lex, is_refutable) );
    case TOK_BRACE_OPEN:
        return Parse_PatternStruct(lex, ps, mv$(path), is_refutable);
    default:
        PUTBACK(tok, lex);
        return AST::Pattern( AST::Pattern::TagValue(), lex.end_span(mv$(ps)), AST::Pattern::Value::make_Named(mv$(path)) );
    }
}

AST::Pattern Parse_PatternReal_Slice(TokenStream& lex, bool is_refutable)
{
    auto ps = lex.start_span();
    Token   tok;

    ::std::vector< ::AST::Pattern>  leading;
    ::std::vector< ::AST::Pattern>  trailing;
    ::AST::PatternBinding   inner_binding;
    bool is_split = false;

    while(GET_TOK(tok, lex) != TOK_SQUARE_CLOSE)
    {
        bool has_binding = true;
        ::AST::PatternBinding  binding;
        if( tok.type() == TOK_RWORD_REF && lex.lookahead(0) == TOK_IDENT && lex.lookahead(1) == TOK_DOUBLE_DOT ) {
            GET_TOK(tok, lex);
            binding = ::AST::PatternBinding( lex.get_ident(mv$(tok)), ::AST::PatternBinding::Type::REF, false );
        }
        else if( tok.type() == TOK_IDENT && lex.lookahead(0) == TOK_DOUBLE_DOT) {
            binding = ::AST::PatternBinding( lex.get_ident(mv$(tok)), ::AST::PatternBinding::Type::MOVE, false );
        }
        else if( tok.type() == TOK_UNDERSCORE && lex.lookahead(0) == TOK_DOUBLE_DOT) {
            // No binding, but switching to trailing
        }
        else if( tok.type() == TOK_DOUBLE_DOT ) {
            // No binding, but switching to trailing
            PUTBACK(tok, lex);
        }
        else {
            has_binding = false;
        }

        if( has_binding ) {
            if(is_split)
                ERROR(lex.end_span(mv$(ps)), E0000, "Multiple instances of .. in a slice pattern");

            inner_binding = mv$(binding);
            is_split = true;
            GET_TOK(tok, lex);  // TOK_DOUBLE_DOT
        }
        else {
            PUTBACK(tok, lex);
            if(!is_split) {
                leading.push_back( Parse_Pattern(lex, is_refutable) );
            }
            else {
                trailing.push_back( Parse_Pattern(lex, is_refutable) );
            }
        }

        if( GET_TOK(tok, lex) != TOK_COMMA )
            break;
    }
    CHECK_TOK(tok, TOK_SQUARE_CLOSE);

    if( is_split )
    {
        return ::AST::Pattern( lex.end_span(mv$(ps)), ::AST::Pattern::Data::make_SplitSlice({ mv$(leading), mv$(inner_binding), mv$(trailing) }) );
    }
    else
    {
        assert( !inner_binding.is_valid() );
        assert( trailing.empty() );
        return ::AST::Pattern( lex.end_span(mv$(ps)), ::AST::Pattern::Data::make_Slice({ mv$(leading) }) );
    }
}

::AST::Pattern::TuplePat Parse_PatternTuple(TokenStream& lex, bool is_refutable)
{
    TRACE_FUNCTION;
    auto sp = lex.start_span();
    Token tok;

    ::std::vector<AST::Pattern> leading;
    while( LOOK_AHEAD(lex) != TOK_PAREN_CLOSE && LOOK_AHEAD(lex) != TOK_DOUBLE_DOT )
    {
        leading.push_back( Parse_Pattern(lex, is_refutable) );

        if( GET_TOK(tok, lex) != TOK_COMMA ) {
            CHECK_TOK(tok, TOK_PAREN_CLOSE);
            return AST::Pattern::TuplePat { mv$(leading), false, {} };
        }
    }

    if( LOOK_AHEAD(lex) != TOK_DOUBLE_DOT )
    {
        GET_TOK(tok, lex);

        CHECK_TOK(tok, TOK_PAREN_CLOSE);
        return AST::Pattern::TuplePat { mv$(leading), false, {} };
    }
    GET_CHECK_TOK(tok, lex, TOK_DOUBLE_DOT);

    ::std::vector<AST::Pattern> trailing;
    if( GET_TOK(tok, lex) == TOK_COMMA )
    {
        while( LOOK_AHEAD(lex) != TOK_PAREN_CLOSE )
        {
            trailing.push_back( Parse_Pattern(lex, is_refutable) );

            if( GET_TOK(tok, lex) != TOK_COMMA ) {
                PUTBACK(tok, lex);
                break;
            }
        }
        GET_TOK(tok, lex);
    }

    CHECK_TOK(tok, TOK_PAREN_CLOSE);
    return ::AST::Pattern::TuplePat { mv$(leading), true, mv$(trailing) };
}

AST::Pattern Parse_PatternStruct(TokenStream& lex, ProtoSpan ps, AST::Path path, bool is_refutable)
{
    TRACE_FUNCTION;
    Token tok;

    // #![feature(relaxed_adts)]
    if( LOOK_AHEAD(lex) == TOK_INTEGER )
    {
        bool split_allowed = false;
        ::std::map<unsigned int, AST::Pattern> pats;
        while( GET_TOK(tok, lex) == TOK_INTEGER )
        {
            unsigned int ofs = static_cast<unsigned int>(tok.intval());
            GET_CHECK_TOK(tok, lex, TOK_COLON);
            auto val = Parse_Pattern(lex, is_refutable);
            if( ! pats.insert( ::std::make_pair(ofs, mv$(val)) ).second ) {
                ERROR(lex.point_span(), E0000, "Duplicate index");
            }

            if( GET_TOK(tok,lex) == TOK_BRACE_CLOSE )
                break;
            CHECK_TOK(tok, TOK_COMMA);
        }
        if( tok.type() == TOK_DOUBLE_DOT ) {
            split_allowed = true;
            GET_TOK(tok, lex);
        }
        CHECK_TOK(tok, TOK_BRACE_CLOSE);

        bool has_split = false;
        ::std::vector<AST::Pattern> leading;
        ::std::vector<AST::Pattern> trailing;
        unsigned int i = 0;
        for(auto& p : pats)
        {
            if( p.first != i ) {
                if( has_split || !split_allowed ) {
                    ERROR(lex.point_span(), E0000, "Missing index " << i);
                }
                has_split = true;
                i = p.first;
            }
            if( ! has_split ) {
                leading.push_back( mv$(p.second) );
            }
            else {
                trailing.push_back( mv$(p.second) );
            }
            i ++;
        }

        return AST::Pattern(AST::Pattern::TagNamedTuple(), lex.end_span(mv$(ps)), mv$(path),  AST::Pattern::TuplePat { mv$(leading), has_split, mv$(trailing) });
    }

    bool is_exhaustive = true;
    ::std::vector< ::std::pair< ::std::string, AST::Pattern> >  subpats;
    do {
        GET_TOK(tok, lex);
        DEBUG("tok = " << tok);
        if( tok.type() == TOK_BRACE_CLOSE )
            break;
        if( tok.type() == TOK_DOUBLE_DOT ) {
            is_exhaustive = false;
            GET_TOK(tok, lex);
            break;
        }

        auto inner_ps = lex.start_span();
        bool is_short_bind = false;
        bool is_box = false;
        auto bind_type = AST::PatternBinding::Type::MOVE;
        bool is_mut = false;
        if( tok.type() == TOK_RWORD_BOX ) {
            is_box = true;
            is_short_bind = true;
            GET_TOK(tok, lex);
        }
        if( tok.type() == TOK_RWORD_REF ) {
            is_short_bind = true;
            GET_TOK(tok, lex);
            if( tok.type() == TOK_RWORD_MUT ) {
                bind_type = AST::PatternBinding::Type::MUTREF;
                GET_TOK(tok, lex);
            }
            else {
                bind_type = AST::PatternBinding::Type::REF;
            }
        }
        else if( tok.type() == TOK_RWORD_MUT ) {
            is_mut = true;
            is_short_bind = true;
            GET_TOK(tok, lex);
        }

        CHECK_TOK(tok, TOK_IDENT);
        auto field_ident = lex.get_ident(mv$(tok));
        ::std::string field_name;
        GET_TOK(tok, lex);

        AST::Pattern    pat;
        if( is_short_bind || tok.type() != TOK_COLON ) {
            PUTBACK(tok, lex);
            pat = AST::Pattern(lex.end_span(inner_ps), {});
            field_name = field_ident.name;
            pat.set_bind(mv$(field_ident), bind_type, is_mut);
            if( is_box )
            {
                pat = AST::Pattern(AST::Pattern::TagBox(), lex.end_span(inner_ps), mv$(pat));
            }
        }
        else {
            CHECK_TOK(tok, TOK_COLON);
            field_name = mv$(field_ident.name);
            pat = Parse_Pattern(lex, is_refutable);
        }

        subpats.push_back( ::std::make_pair(mv$(field_name), mv$(pat)) );
    } while( GET_TOK(tok, lex) == TOK_COMMA );
    CHECK_TOK(tok, TOK_BRACE_CLOSE);

    return AST::Pattern(AST::Pattern::TagStruct(), lex.end_span(mv$(ps)), ::std::move(path), ::std::move(subpats), is_exhaustive);
}

