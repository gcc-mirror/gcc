/*
 * MRustC - Mutabah's Rust Compiler
 * - By John Hodge (Mutabah/thePowersGang)
 *
 * ast/macro.hpp
 * - AST representation of a macro invocation
 */
#ifndef _AST_MACRO_HPP_
#define _AST_MACRO_HPP_

#include "../parse/tokentree.hpp"
#include <span.hpp>
#include "attrs.hpp"

namespace AST {

class MacroInvocation
{
    Span    m_span;

    ::std::string   m_macro_name;
    ::std::string   m_ident;
    TokenTree   m_input;
public:
    MacroInvocation(MacroInvocation&&) = default;
    MacroInvocation& operator=(MacroInvocation&&) = default;
    MacroInvocation(const MacroInvocation&) = delete;
    MacroInvocation& operator=(const MacroInvocation&) = delete;

    MacroInvocation()
    {
    }

    MacroInvocation(Span span, ::std::string macro, ::std::string ident, TokenTree input):
        m_span( mv$(span) ),
        m_macro_name( mv$(macro) ),
        m_ident( mv$(ident) ),
        m_input( mv$(input) )
    {
    }

    MacroInvocation clone() const;

    void clear() {
        m_macro_name = "";
        m_ident = "";
        m_input = TokenTree();
    }

    const Span& span() const { return m_span; }
    const ::std::string& name() const { return m_macro_name; }

    const ::std::string& input_ident() const { return m_ident; }
    const TokenTree& input_tt() const { return m_input; }
          TokenTree& input_tt()       { return m_input; }

    friend ::std::ostream& operator<<(::std::ostream& os, const MacroInvocation& x) {
        os << x.m_macro_name << "! " << x.m_ident << x.m_input;
        return os;
    }
};


}

#endif

