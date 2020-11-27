/*
 * MRustC - Mutabah's Rust Compiler
 * - By John Hodge (Mutabah/thePowersGang)
 *
 * parse/parseerror.hpp
 * - Exception classes for parsing/lexing errors
 */
#ifndef PARSEERROR_HPP_INCLUDED
#define PARSEERROR_HPP_INCLUDED

#include <stdexcept>
#include "tokenstream.hpp"
#include <compile_error.hpp>

namespace ParseError {

using CompileError::Generic;
using CompileError::BugCheck;
using CompileError::Todo;

class BadChar:
    public CompileError::Base
{
    //char    m_char;
public:
    BadChar(const TokenStream& lex, char character);
    virtual ~BadChar() throw ();

};

class Unexpected:
    public CompileError::Base
{
    Token   m_tok;
public:
    Unexpected(const TokenStream& lex, const Token& tok);
    Unexpected(const TokenStream& lex, const Token& tok, Token exp);
    Unexpected(const TokenStream& lex, const Token& tok, ::std::vector<eTokenType> exp);
    virtual ~Unexpected() throw ();

};

#define ASSERT(lex, cnd)    do { if( !(cnd) ) throw CompileError::BugCheck(lex, "Assertion failed: " __FILE__ " - " #cnd); } while(0)

}

#endif // PARSEERROR_HPP_INCLUDED
