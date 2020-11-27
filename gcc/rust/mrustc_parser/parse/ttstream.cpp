/*
 * MRustC - Rust Compiler
 * - By John Hodge (Mutabah/thePowersGang)
 *
 * parse/ttstream.cpp
 * - Token-Tree backed token streams
 */
#include "ttstream.hpp"
#include <common.hpp>

TTStream::TTStream(Span parent, const TokenTree& input_tt):
    m_parent_span( new Span(mv$(parent)) )
{
    DEBUG("input_tt = [" << input_tt << "]");
    m_stack.push_back( ::std::make_pair(0, &input_tt) );
}
TTStream::~TTStream()
{
}
Token TTStream::realGetToken()
{
    while(m_stack.size() > 0)
    {
        // If current index is above TT size, go up
        unsigned int& idx = m_stack.back().first;
        assert( m_stack.back().second );
        const TokenTree& tree = *m_stack.back().second;

        if(idx == 0 && tree.is_token()) {
            idx ++;
            m_hygiene_ptr = &tree.hygiene();
            return tree.tok();
        }

        if(idx < tree.size())
        {
            const TokenTree&    subtree = tree[idx];
            idx ++;
            if( subtree.size() == 0 ) {
                m_hygiene_ptr = &subtree.hygiene();
                return subtree.tok().clone();
            }
            else {
                m_stack.push_back( ::std::make_pair(0, &subtree) );
            }
        }
        else {
            m_stack.pop_back();
        }
    }
    //m_hygiene = nullptr;
    return Token(TOK_EOF);
}
Position TTStream::getPosition() const
{
    // TODO: Position associated with the previous/next token?
    return Position("TTStream", 0,0);
}
Ident::Hygiene TTStream::realGetHygiene() const
{
    // Empty.
    if(!m_hygiene_ptr)
        return Ident::Hygiene();
    return *m_hygiene_ptr;
}


TTStreamO::TTStreamO(Span parent, TokenTree input_tt):
    m_input_tt( mv$(input_tt) ),
    m_parent_span( new Span(mv$(parent)) )
{
    m_stack.push_back( ::std::make_pair(0, nullptr) );
}
TTStreamO::~TTStreamO()
{
}
Token TTStreamO::realGetToken()
{
    while(m_stack.size() > 0)
    {
        // If current index is above TT size, go up
        unsigned int& idx = m_stack.back().first;
        TokenTree& tree = (m_stack.back().second ? *m_stack.back().second : m_input_tt);

        if(idx == 0 && tree.is_token()) {
            idx ++;
            m_last_pos = tree.tok().get_pos();
            m_hygiene_ptr = &tree.hygiene();
            return mv$(tree.tok());
        }

        if(idx < tree.size())
        {
            TokenTree& subtree = tree[idx];
            idx ++;
            if( subtree.size() == 0 ) {
                m_last_pos = subtree.tok().get_pos();
                m_hygiene_ptr = &subtree.hygiene();
                return mv$( subtree.tok() );
            }
            else {
                m_stack.push_back( ::std::make_pair(0, &subtree) );
            }
        }
        else {
            m_stack.pop_back();
        }
    }
    return Token(TOK_EOF);
}
Position TTStreamO::getPosition() const
{
    return m_last_pos;
}
Ident::Hygiene TTStreamO::realGetHygiene() const
{
    // Empty.
    if(!m_hygiene_ptr)
        return Ident::Hygiene();
    return *m_hygiene_ptr;
}
