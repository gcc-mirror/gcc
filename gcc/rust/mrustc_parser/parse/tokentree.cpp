/*
 * MRustC - Rust Compiler
 * - By John Hodge (Mutabah/thePowersGang)
 *
 * parse/tokentree.cpp
 * - Token Tree (collection of tokens)
 */
#include "tokentree.hpp"
#include <common.hpp>

TokenTree TokenTree::clone() const
{
    if( m_subtrees.size() == 0 ) {
        return TokenTree(m_hygiene, m_tok.clone());
    }
    else {
        ::std::vector< TokenTree>   ents;
        ents.reserve( m_subtrees.size() );
        for(const auto& sub : m_subtrees)
            ents.push_back( sub.clone() );
        return TokenTree( m_hygiene, mv$(ents) );
    }
}

::std::ostream& operator<<(::std::ostream& os, const TokenTree& tt)
{
    if( tt.m_subtrees.size() == 0 )
    {
        switch(tt.m_tok.type())
        {
        case TOK_IDENT:
        case TOK_LIFETIME:
            os << "/*" << tt.m_hygiene << "*/";
            break;
        default:
            if( TOK_INTERPOLATED_IDENT <= tt.m_tok.type() && tt.m_tok.type() <= TOK_INTERPOLATED_ITEM ) {
                os << "/*int*/";
            }
            break;
        }
        return os << tt.m_tok.to_str();
    }
    else {
        os << "/*" << tt.m_hygiene << " TT*/";
        // NOTE: All TTs (except the outer tt on a macro invocation) include the grouping
        bool first = true;
        for(const auto& i : tt.m_subtrees) {
            if(!first)
                os << " ";
            os << i;
            first = false;
        }
        return os;
    }
}
