/*
 * MRustC - Rust Compiler
 * - By John Hodge (Mutabah/thePowersGang)
 *
 * parse/paths.cpp
 * - Parsing for module paths
 */
#include "parseerror.hpp"
#include "common.hpp"
#include "../ast/ast.hpp"

AST::Path   Parse_Path(TokenStream& lex, eParsePathGenericMode generic_mode);
AST::Path   Parse_Path(TokenStream& lex, bool is_abs, eParsePathGenericMode generic_mode);
::std::vector<AST::PathNode> Parse_PathNodes(TokenStream& lex, eParsePathGenericMode generic_mode);
AST::PathParams Parse_Path_GenericList(TokenStream& lex);

AST::Path Parse_Path(TokenStream& lex, eParsePathGenericMode generic_mode)
{
    TRACE_FUNCTION_F("generic_mode="<<generic_mode);

    Token   tok;
    switch( GET_TOK(tok, lex) )
    {
    case TOK_INTERPOLATED_PATH:
        return mv$(tok.frag_path());

    case TOK_RWORD_SELF:
        GET_CHECK_TOK(tok, lex, TOK_DOUBLE_COLON);
        return AST::Path(AST::Path::TagSelf(), Parse_PathNodes(lex, generic_mode));

    case TOK_RWORD_SUPER: {
        GET_CHECK_TOK(tok, lex, TOK_DOUBLE_COLON);
        unsigned int count = 1;
        while( LOOK_AHEAD(lex) == TOK_RWORD_SUPER ) {
            count += 1;
            GET_TOK(tok, lex);
            GET_CHECK_TOK(tok, lex, TOK_DOUBLE_COLON);
        }
        return AST::Path(AST::Path::TagSuper(), count, Parse_PathNodes(lex, generic_mode));
        }

    case TOK_DOUBLE_COLON:
        return Parse_Path(lex, true, generic_mode);

    case TOK_DOUBLE_LT:
        lex.putback( Token(TOK_LT) );
    case TOK_LT: {
        TypeRef ty = Parse_Type(lex, true);  // Allow trait objects without parens
        if( GET_TOK(tok, lex) == TOK_RWORD_AS ) {
            ::AST::Path trait;
            if( GET_TOK(tok, lex) == TOK_DOUBLE_COLON ) {
                trait = Parse_Path(lex, true, PATH_GENERIC_TYPE);
            }
            else {
                PUTBACK(tok, lex);
                trait = Parse_Path(lex, false, PATH_GENERIC_TYPE);
            }
            GET_CHECK_TOK(tok, lex, TOK_GT);
            GET_CHECK_TOK(tok, lex, TOK_DOUBLE_COLON);
            return AST::Path(AST::Path::TagUfcs(), mv$(ty), mv$(trait), Parse_PathNodes(lex, generic_mode));
        }
        else {
            PUTBACK(tok, lex);
            GET_CHECK_TOK(tok, lex, TOK_GT);
            // TODO: Terminating the "path" here is sometimes valid?
            GET_CHECK_TOK(tok, lex, TOK_DOUBLE_COLON);
            // NOTE: <Foo>::BAR is actually `<Foo as _>::BAR` (in mrustc parleance)
            //return AST::Path(AST::Path::TagUfcs(), mv$(ty), Parse_PathNodes(lex, generic_mode));
            return AST::Path(AST::Path::TagUfcs(), mv$(ty), AST::Path(), Parse_PathNodes(lex, generic_mode));
        }
        throw ""; }

    default:
        PUTBACK(tok, lex);
        return Parse_Path(lex, false, generic_mode);
    }
}
AST::Path Parse_Path(TokenStream& lex, bool is_abs, eParsePathGenericMode generic_mode)
{
    Token   tok;
    if( is_abs )
    {
        if( GET_TOK(tok, lex) == TOK_STRING ) {
            ::std::string   cratename = tok.str();
            GET_CHECK_TOK(tok, lex, TOK_DOUBLE_COLON);
            return AST::Path(cratename, Parse_PathNodes(lex, generic_mode));
        }
        else {
            PUTBACK(tok, lex);
            return AST::Path("", Parse_PathNodes(lex, generic_mode));
        }
    }
    else {
        // TODO: TOK_INTERPOLATED_IDENT?
        GET_CHECK_TOK(tok, lex, TOK_IDENT);
        auto hygine = lex.getHygiene();
        PUTBACK(tok, lex);
        return AST::Path(AST::Path::TagRelative(), mv$(hygine), Parse_PathNodes(lex, generic_mode));
    }
}

::std::vector<AST::PathNode> Parse_PathNodes(TokenStream& lex, eParsePathGenericMode generic_mode)
{
    TRACE_FUNCTION_F("generic_mode="<<generic_mode);

    Token tok;
    ::std::vector<AST::PathNode>    ret;

    tok = lex.getToken();
    while(true)
    {
        ::AST::PathParams   params;

        CHECK_TOK(tok, TOK_IDENT);
        auto component = mv$( tok.str() );

        GET_TOK(tok, lex);
        if( generic_mode == PATH_GENERIC_TYPE )
        {
            if( tok.type() == TOK_LT || tok.type() == TOK_DOUBLE_LT )
            {
                // HACK! Handle breaking << into < <
                if( tok.type() == TOK_DOUBLE_LT )
                    lex.putback( Token(TOK_LT) );

                // Type-mode generics "::path::to::Type<A,B>"
                params = Parse_Path_GenericList(lex);
                GET_TOK(tok, lex);
            }
            // HACK - 'Fn*(...) -> ...' notation
            else if( tok.type() == TOK_PAREN_OPEN )
            {
                auto ps = lex.start_span();
                DEBUG("Fn() hack");
                ::std::vector<TypeRef>  args;
                if( GET_TOK(tok, lex) == TOK_PAREN_CLOSE )
                {
                    // Empty list
                }
                else
                {
                    PUTBACK(tok, lex);
                    do {
                        // TODO: Trailing commas
                        args.push_back( Parse_Type(lex) );
                    } while( GET_TOK(tok, lex) == TOK_COMMA );
                }
                CHECK_TOK(tok, TOK_PAREN_CLOSE);

                TypeRef ret_type = TypeRef( TypeRef::TagUnit(), Span(tok.get_pos()) );
                if( GET_TOK(tok, lex) == TOK_THINARROW ) {
                    ret_type = Parse_Type(lex, false);
                }
                else {
                    PUTBACK(tok, lex);
                }
                DEBUG("- Fn("<<args<<")->"<<ret_type<<"");

                // Encode into path, by converting Fn(A,B)->C into Fn<(A,B),Ret=C>
                params = ::AST::PathParams {
                    {},
                    ::make_vec1( TypeRef(TypeRef::TagTuple(), lex.end_span(ps), mv$(args)) ),
                    ::make_vec1( ::std::make_pair( ::std::string("Output"), mv$(ret_type) ) )
                    };

                GET_TOK(tok, lex);
            }
            else
            {
            }
        }
        if( tok.type() != TOK_DOUBLE_COLON ) {
            ret.push_back( AST::PathNode(component, mv$(params)) );
            break;
        }
        tok = lex.getToken();
        if( generic_mode == PATH_GENERIC_EXPR && (tok.type() == TOK_LT || tok.type() == TOK_DOUBLE_LT) )
        {
            // HACK! Handle breaking << into < <
            if( tok.type() == TOK_DOUBLE_LT )
                lex.putback( Token(TOK_LT) );

            // Expr-mode generics "::path::to::function::<Type1,Type2>(arg1, arg2)"
            params = Parse_Path_GenericList(lex);
            if( GET_TOK(tok, lex) != TOK_DOUBLE_COLON ) {
                ret.push_back( AST::PathNode(component, mv$(params)) );
                // Break out of loop down to return
                break;
            }
            // Match with CHECK_TOK at start of loop
            GET_TOK(tok, lex);
        }
        ret.push_back( AST::PathNode(component, mv$(params)) );
    }
    PUTBACK(tok, lex);
    DEBUG("ret = " << ret);
    return ret;
}
/// Parse a list of parameters within a path
::AST::PathParams Parse_Path_GenericList(TokenStream& lex)
{
    TRACE_FUNCTION;
    Token   tok;

    ::std::vector<TypeRef>  types;
    ::std::vector<AST::LifetimeRef>   lifetimes;
    ::std::vector< ::std::pair< ::std::string, TypeRef > > assoc_bounds;

    do {
        if( LOOK_AHEAD(lex) == TOK_GT || LOOK_AHEAD(lex) == TOK_DOUBLE_GT || LOOK_AHEAD(lex) == TOK_GTE || LOOK_AHEAD(lex) == TOK_DOUBLE_GT_EQUAL ) {
            GET_TOK(tok, lex);
            break;
        }
        switch(GET_TOK(tok, lex))
        {
        case TOK_LIFETIME:
            lifetimes.push_back(AST::LifetimeRef(/*lex.point_span(),*/ lex.get_ident(mv$(tok)) ));
            break;
        case TOK_IDENT:
            if( LOOK_AHEAD(lex) == TOK_EQUAL )
            {
                ::std::string name = mv$(tok.str());
                GET_CHECK_TOK(tok, lex, TOK_EQUAL);
                assoc_bounds.push_back( ::std::make_pair( mv$(name), Parse_Type(lex,false) ) );
                break;
            }
        default:
            PUTBACK(tok, lex);
            types.push_back( Parse_Type(lex) );
            break;
        }
    } while( GET_TOK(tok, lex) == TOK_COMMA );

    // HACK: Split >> into >
    if(tok.type() == TOK_DOUBLE_GT_EQUAL) {
        lex.putback(Token(TOK_GTE));
    }
    else if(tok.type() == TOK_GTE) {
        lex.putback(Token(TOK_EQUAL));
    }
    else if(tok.type() == TOK_DOUBLE_GT) {
        lex.putback(Token(TOK_GT));
    }
    else {
        CHECK_TOK(tok, TOK_GT);
    }

    return ::AST::PathParams {
        mv$( lifetimes ),
        mv$( types ),
        mv$( assoc_bounds )
        };
}

