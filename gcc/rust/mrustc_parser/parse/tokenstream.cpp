/*
 * MRustC - Rust Compiler
 * - By John Hodge (Mutabah/thePowersGang)
 *
 * parse/tokenstream.cpp
 * - TokenStream - Parser token source interface
 */
#include "tokenstream.hpp"
#include <common.hpp>
#include "parseerror.hpp"

const bool DEBUG_PRINT_TOKENS = false;
//const bool DEBUG_PRINT_TOKENS = true;
//#define DEBUG_PRINT_TOKENS  debug_enabled("Lexer Tokens")
//#define FULL_TRACE

TokenStream::TokenStream():
    m_cache_valid(false)
{
}
TokenStream::~TokenStream()
{
}

Token TokenStream::innerGetToken()
{
    Token ret = this->realGetToken();
    if( ret != TOK_EOF && ret.get_pos().filename == "" )
        ret.set_pos( this->getPosition() );
    //DEBUG("ret.get_pos() = " << ret.get_pos());
    return ret;
}
Token TokenStream::getToken()
{
    if( m_cache_valid )
    {
#ifdef FULL_TRACE
        DEBUG("<<< " << m_cache << " (cache)");
#endif
        m_cache_valid = false;
        return mv$(m_cache);
    }
    else if( m_lookahead.size() )
    {
        Token ret = mv$( m_lookahead.front().first );
        m_hygiene = m_lookahead.front().second;
        m_lookahead.erase(m_lookahead.begin());
#ifdef FULL_TRACE
        DEBUG("<<< " << ret << " (lookahead)");
#endif
        if( DEBUG_PRINT_TOKENS ) {
            ::std::cout << "getToken[" << typeid(*this).name() << "] - " << ret.get_pos() << "-" << ret << ::std::endl;
        }
        return ret;
    }
    else
    {
        Token ret = this->innerGetToken();
        m_hygiene = this->realGetHygiene();
#ifdef FULL_TRACE
        DEBUG("<<< " << ret << " (new)");
#endif
        if( DEBUG_PRINT_TOKENS ) {
            ::std::cout << "getToken[" << typeid(*this).name() << "] - " << ret.get_pos() << "-" << ret << ::std::endl;
        }
        return ret;
    }
}
void TokenStream::putback(Token tok)
{
    if( m_cache_valid )
    {
        DEBUG("" << getPosition() << " - Double putback: " << tok << " but " << m_cache);
        throw ParseError::BugCheck("Double putback");
    }
    else
    {
#ifdef FULL_TRACE
        DEBUG(">>> " << tok);
#endif
        m_cache_valid = true;
        m_cache = mv$(tok);
    }
}

eTokenType TokenStream::lookahead(unsigned int i)
{
    const unsigned int MAX_LOOKAHEAD = 3;

    if( m_cache_valid )
    {
        if( i == 0 )
            return m_cache.type();
        i --;
    }

    if( i >= MAX_LOOKAHEAD )
        throw ParseError::BugCheck("Excessive lookahead");

    while( i >= m_lookahead.size() )
    {
        DEBUG("lookahead - read #" << m_lookahead.size());
        auto tok = this->innerGetToken();
        auto hygiene = this->realGetHygiene();
        m_lookahead.push_back( ::std::make_pair(mv$(tok), mv$(hygiene)) );
    }

    DEBUG("lookahead(" << i << ") = " << m_lookahead[i]);
    return m_lookahead[i].first.type();
}

Ident::Hygiene TokenStream::getHygiene() const
{
    return m_hygiene;
}

ProtoSpan TokenStream::start_span() const
{
    auto p = this->getPosition();
    return ProtoSpan {
        p.filename,
        p.line, p.ofs
        };
}
Span TokenStream::end_span(ProtoSpan ps) const
{
    auto p = this->getPosition();
    auto rv = Span( ::std::move(ps.filename),  ps.start_line, ps.start_ofs,  p.line, p.ofs );
    rv.outer_span = this->outerSpan();
    return rv;
}
Span TokenStream::point_span() const
{
    Span rv = this->getPosition();
    rv.outer_span = this->outerSpan();
    return rv;
}
Ident TokenStream::get_ident(Token tok) const
{
    if(tok.type() == TOK_IDENT) {
        return Ident(getHygiene(), tok.str());
    }
    else if(tok.type() == TOK_LIFETIME) {
        // TODO: Maybe only when it's explicitly asked for?
        return Ident(getHygiene(), tok.str());
    }
    else if( tok.type() == TOK_INTERPOLATED_IDENT ) {
        TODO(getPosition(), "get_ident from TOK_INTERPOLATED_IDENT");
    }
    else {
        throw ParseError::Unexpected(*this, tok);
    }
}
