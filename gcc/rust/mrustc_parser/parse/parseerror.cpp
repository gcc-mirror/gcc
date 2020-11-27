/*
 * MRustC - Rust Compiler
 * - By John Hodge (Mutabah/thePowersGang)
 *
 * parse/parseerror.cpp
 * - Exceptions thrown for different types of parsing errors
 */
#include "parseerror.hpp"
#include <iostream>

CompileError::Base::~Base() throw()
{
}

CompileError::Generic::Generic(::std::string message):
    m_message(message)
{
    ::std::cout << "Generic(" << message << ")" << ::std::endl;
}
CompileError::Generic::Generic(const TokenStream& lex, ::std::string message)
{
    ::std::cout << lex.point_span() << ": Generic(" << message << ")" << ::std::endl;
}

CompileError::BugCheck::BugCheck(const TokenStream& lex, ::std::string message):
    m_message(message)
{
    ::std::cout << lex.point_span() << "BugCheck(" << message << ")" << ::std::endl;
}
CompileError::BugCheck::BugCheck(::std::string message):
    m_message(message)
{
    ::std::cout << "BugCheck(" << message << ")" << ::std::endl;
}

CompileError::Todo::Todo(::std::string message):
    m_message(message)
{
    ::std::cout << "Todo(" << message << ")" << ::std::endl;
}
CompileError::Todo::Todo(const TokenStream& lex, ::std::string message):
    m_message(message)
{
    ::std::cout << lex.point_span() << ": Todo(" << message << ")" << ::std::endl;
}
CompileError::Todo::~Todo() throw()
{
}

ParseError::BadChar::BadChar(const TokenStream& lex, char character)
{
    ::std::cout << lex.point_span() << ": BadChar(" << character << ")" << ::std::endl;
}
ParseError::BadChar::~BadChar() throw()
{
}

ParseError::Unexpected::Unexpected(const TokenStream& lex, const Token& tok)//:
//    m_tok( mv$(tok) )
{
    Span pos = tok.get_pos();
    if(pos.filename == "")
        pos = lex.point_span();
    ::std::cout << pos << ": Unexpected(" << tok << ")" << ::std::endl;
}
ParseError::Unexpected::Unexpected(const TokenStream& lex, const Token& tok, Token exp)//:
//    m_tok( mv$(tok) )
{
    Span pos = tok.get_pos();
    if(pos.filename == "")
        pos = lex.point_span();
    ::std::cout << pos << ": Unexpected(" << tok << ", " << exp << ")" << ::std::endl;
}
ParseError::Unexpected::Unexpected(const TokenStream& lex, const Token& tok, ::std::vector<eTokenType> exp)
{
    Span pos = tok.get_pos();
    if(pos.filename == "")
        pos = lex.point_span();
    ::std::cout << pos << ": Unexpected " << tok << ", expected ";
    bool f = true;
    for(auto v: exp) {
        if(!f)
            ::std::cout << " or ";
        f = false;
        ::std::cout << Token::typestr(v);
    }
    ::std::cout << ::std::endl;
}
ParseError::Unexpected::~Unexpected() throw()
{
}
