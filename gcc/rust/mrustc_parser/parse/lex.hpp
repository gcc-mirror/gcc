/*
 * MRustC - Rust Compiler
 * - By John Hodge (Mutabah/thePowersGang)
 *
 * parse/lex.hpp
 * - Lexer header
 */
#ifndef LEX_HPP_INCLUDED
#define LEX_HPP_INCLUDED

#include <string>
#include <fstream>
#include "tokenstream.hpp"

struct Codepoint {
    uint32_t    v;
    Codepoint(): v(0) { }
    Codepoint(uint32_t v): v(v) { }
    bool isspace() const;
    bool isdigit() const;
    bool isxdigit() const;
    bool operator==(char x) { return v == static_cast<uint32_t>(x); }
    bool operator!=(char x) { return v != static_cast<uint32_t>(x); }
    bool operator==(Codepoint x) { return v == x.v; }
    bool operator!=(Codepoint x) { return v != x.v; }
};
extern ::std::string& operator+=(::std::string& s, const Codepoint& cp);
extern ::std::ostream& operator<<(::std::ostream& s, const Codepoint& cp);

extern Token Lex_FindOperator(const ::std::string& s);
extern Token Lex_FindReservedWord(const ::std::string& s);

typedef Codepoint   uchar;

class Lexer:
    public TokenStream
{
    RcString    m_path;
    unsigned int m_line;
    unsigned int m_line_ofs;

    ::std::ifstream m_istream;
    bool    m_last_char_valid;
    Codepoint   m_last_char;
    ::std::vector<Token>    m_next_tokens;

    Ident::Hygiene m_hygiene;
public:
    Lexer(const ::std::string& filename);

    Position getPosition() const override;
    Ident::Hygiene realGetHygiene() const override;
    Token realGetToken() override;

private:
    Token getTokenInt();

    signed int getSymbol();
    Token getTokenInt_RawString(bool is_byte);
    Token getTokenInt_Identifier(Codepoint ch, Codepoint ch2='\0');
    double parseFloat(uint64_t whole);
    uint32_t parseEscape(char enclosing);

    void push_hygine() override {
        m_hygiene = Ident::Hygiene::new_scope_chained(m_hygiene);
    }
    void pop_hygine() override {
        m_hygiene = m_hygiene.get_parent();
    }

    void ungetc();
    Codepoint getc_num();
    Codepoint getc();
    Codepoint getc_cp();
    char getc_byte();

    class EndOfFile {};
};

#endif // LEX_HPP_INCLUDED
