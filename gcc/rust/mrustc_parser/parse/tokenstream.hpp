/*
 * MRustC - Rust Compiler
 * - By John Hodge (Mutabah/thePowersGang)
 *
 * parse/tokenstream.hpp
 * - Parser stream (TokenStream) header
 */
#pragma once

#include <iostream>
#include <vector>
#include <span.hpp>
#include <debug.hpp>
#include <ident.hpp>
#include "token.hpp"

namespace AST {
    class Module;
    class AttributeList;
}

/// State the parser needs to pass down via a second channel.
struct ParseState
{
    // Used for "for/if/while" to handle ambiguity
    bool disallow_struct_literal = false;
    // A debugging hook that disables expansion of macros
    bool no_expand_macros = false;

    ::AST::Module*  module = nullptr;
    ::AST::AttributeList*   parent_attrs = nullptr;

    ::AST::Module& get_current_mod() {
        assert(this->module);
        return *this->module;
    }

    friend ::std::ostream& operator<<(::std::ostream& os, const ParseState& ps) {
        os << "ParseState {";
        if(ps.disallow_struct_literal)  os << " disallow_struct_literal";
        if(ps.no_expand_macros)  os << " no_expand_macros";
        os << " }";
        return os;
    }
};

class TokenStream
{
    friend class TTLexer;   // needs access to internals to know what was consumed

    bool    m_cache_valid;
    Token   m_cache;
    Ident::Hygiene  m_hygiene;
    ::std::vector< ::std::pair<Token, Ident::Hygiene> > m_lookahead;
    ParseState  m_parse_state;
public:
    TokenStream();
    virtual ~TokenStream();
    Token   getToken();
    void    putback(Token tok);
    eTokenType  lookahead(unsigned int count);

    Ident::Hygiene getHygiene() const;
    virtual void push_hygine() {}
    virtual void pop_hygine() {}

    ParseState& parse_state() { return m_parse_state; }

    ProtoSpan   start_span() const;
    Span    end_span(ProtoSpan ps) const;
    Span    point_span() const;

    Ident get_ident(Token tok) const;

protected:
    virtual Position getPosition() const = 0;
    virtual ::std::shared_ptr<Span> outerSpan() const { return ::std::shared_ptr<Span>(0); }
    virtual Token   realGetToken() = 0;
    virtual Ident::Hygiene realGetHygiene() const = 0;
private:
    Token innerGetToken();
};

class SavedParseState
{
    TokenStream&    m_lex;
    ParseState  m_state;
public:
    SavedParseState(TokenStream& lex, ParseState state):
        m_lex(lex),
        m_state(state)
    {
    }
    ~SavedParseState()
    {
        DEBUG("Restoring " << m_state);
        m_lex.parse_state() = m_state;
    }
};

#define SET_MODULE(lex, mod)    SavedParseState _sps(lex, lex.parse_state()); lex.parse_state().module = &(mod)
#define SET_ATTRS(lex, attrs)    SavedParseState _sps(lex, lex.parse_state()); lex.parse_state().parent_attrs = &(attrs)
#define SET_PARSE_FLAG(lex, flag)    SavedParseState _sps(lex, lex.parse_state()); lex.parse_state().flag = true
#define CLEAR_PARSE_FLAG(lex, flag)    SavedParseState _sps(lex, lex.parse_state()); lex.parse_state().flag = false
#define CHECK_PARSE_FLAG(lex, flag) (lex.parse_state().flag == true)
