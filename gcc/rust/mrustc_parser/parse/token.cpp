/*
 * MRustC - Rust Compiler
 * - By John Hodge (Mutabah/thePowersGang)
 */
#include "token.hpp"
#include <common.hpp>
#include <parse/parseerror.hpp>
#include "interpolated_fragment.hpp"
#include <ast/types.hpp>
#include <ast/ast.hpp>
#include <ast/expr.hpp> // for reasons

Token::~Token()
{
    switch(m_type)
    {
    case TOK_INTERPOLATED_TYPE:
        delete reinterpret_cast<TypeRef*>(m_data.as_Fragment());
        break;
    case TOK_INTERPOLATED_PATTERN:
        delete reinterpret_cast<AST::Pattern*>(m_data.as_Fragment());
        break;
    case TOK_INTERPOLATED_PATH:
        delete reinterpret_cast<AST::Path*>(m_data.as_Fragment());
        break;
    case TOK_INTERPOLATED_EXPR:
        delete reinterpret_cast<AST::ExprNode*>(m_data.as_Fragment());
        break;
    case TOK_INTERPOLATED_STMT:
        delete reinterpret_cast<AST::ExprNode*>(m_data.as_Fragment());
        break;
    case TOK_INTERPOLATED_BLOCK:
        delete reinterpret_cast<AST::ExprNode*>(m_data.as_Fragment());
        break;
    case TOK_INTERPOLATED_META:
        delete reinterpret_cast<AST::Attribute*>(m_data.as_Fragment());
        break;
    default:
        break;
    }

}

Token::Token():
    m_type(TOK_NULL)
{
}
Token::Token(enum eTokenType type):
    m_type(type)
{
}
Token::Token(enum eTokenType type, ::std::string str):
    m_type(type),
    m_data(Data::make_String(mv$(str)))
{
}
Token::Token(uint64_t val, enum eCoreType datatype):
    m_type(TOK_INTEGER),
    m_data( Data::make_Integer({datatype, val}) )
{
}
Token::Token(double val, enum eCoreType datatype):
    m_type(TOK_FLOAT),
    m_data( Data::make_Float({datatype, val}) )
{
}
Token::Token(const InterpolatedFragment& frag)
{
    switch(frag.m_type)
    {
    case InterpolatedFragment::TT:  throw "";
    case InterpolatedFragment::TYPE:
        m_type = TOK_INTERPOLATED_TYPE;
        m_data = new TypeRef( reinterpret_cast<const TypeRef*>(frag.m_ptr)->clone() );
        break;
    case InterpolatedFragment::PAT:
        m_type = TOK_INTERPOLATED_PATTERN;
        m_data = new AST::Pattern( reinterpret_cast<const AST::Pattern*>(frag.m_ptr)->clone() );
        break;
    case InterpolatedFragment::PATH:
        m_type = TOK_INTERPOLATED_PATH;
        m_data = new AST::Path( *reinterpret_cast<const AST::Path*>(frag.m_ptr) );
        break;
    case InterpolatedFragment::EXPR:
        m_type = TOK_INTERPOLATED_EXPR; if(0)
    case InterpolatedFragment::STMT:
        m_type = TOK_INTERPOLATED_STMT; if(0)
    case InterpolatedFragment::BLOCK:
        m_type = TOK_INTERPOLATED_BLOCK;

        m_data = reinterpret_cast<const AST::ExprNode*>(frag.m_ptr)->clone().release();
        break;
    case InterpolatedFragment::META:
        m_type = TOK_INTERPOLATED_META;
        m_data = new AST::Attribute( reinterpret_cast<const AST::Attribute*>(frag.m_ptr)->clone() );
        break;
    case InterpolatedFragment::ITEM: {
        m_type = TOK_INTERPOLATED_ITEM;
        const auto& named = *reinterpret_cast<const AST::Named<AST::Item>*>(frag.m_ptr);
        ::AST::Item item = named.data.clone();
        m_data = new AST::Named<AST::Item>( named.name, mv$(item), named.is_pub );
        break; }
    }
}
Token::Token(TagTakeIP, InterpolatedFragment frag)
{
    switch(frag.m_type)
    {
    case InterpolatedFragment::TT:  throw "";
    case InterpolatedFragment::TYPE:
        m_type = TOK_INTERPOLATED_TYPE;
        m_data = new TypeRef( mv$(*reinterpret_cast<TypeRef*>(frag.m_ptr)) );
        break;
    case InterpolatedFragment::PAT:
        m_type = TOK_INTERPOLATED_PATTERN;
        m_data = new AST::Pattern( mv$(*reinterpret_cast<AST::Pattern*>(frag.m_ptr)) );
        break;
    case InterpolatedFragment::PATH:
        m_type = TOK_INTERPOLATED_PATH;
        m_data = new AST::Path( mv$(*reinterpret_cast<AST::Path*>(frag.m_ptr)) );
        break;
    case InterpolatedFragment::EXPR:
        m_type = TOK_INTERPOLATED_EXPR; if(0)
    case InterpolatedFragment::STMT:
        m_type = TOK_INTERPOLATED_STMT; if(0)
    case InterpolatedFragment::BLOCK:
        m_type = TOK_INTERPOLATED_BLOCK;

        m_data = reinterpret_cast<AST::ExprNode*>(frag.m_ptr);
        frag.m_ptr = nullptr;
        break;
    case InterpolatedFragment::ITEM:
        m_type = TOK_INTERPOLATED_ITEM;
        m_data = frag.m_ptr;
        frag.m_ptr = nullptr;
        break;
    case InterpolatedFragment::META:
        m_type = TOK_INTERPOLATED_META;
        m_data = frag.m_ptr;
        frag.m_ptr = nullptr;
        break;
    }
}

Token::Token(const Token& t):
    m_type(t.m_type),
    m_data( Data::make_None({}) ),
    m_pos( t.m_pos )
{
    assert( t.m_data.tag() != Data::TAGDEAD );
    TU_MATCH(Data, (t.m_data), (e),
    (None,  ),
    (String,    m_data = Data::make_String(e); ),
    (Integer,   m_data = Data::make_Integer(e);),
    (Float, m_data = Data::make_Float(e);),
    (Fragment, BUG(t.m_pos, "Attempted to copy a fragment - " << t);)
    )
}
Token Token::clone() const
{
    Token   rv(m_type);
    rv.m_pos = m_pos;

    assert( m_data.tag() != Data::TAGDEAD );
    TU_MATCH(Data, (m_data), (e),
    (None,
        ),
    (String,
        rv.m_data = Data::make_String(e);
        ),
    (Integer,
        rv.m_data = Data::make_Integer(e);
        ),
    (Float,
        rv.m_data = Data::make_Float(e);
        ),
    (Fragment,
        assert(e);
        switch(m_type)
        {
        case TOK_INTERPOLATED_TYPE:
            rv.m_data = new TypeRef( reinterpret_cast<TypeRef*>(e)->clone() );
            break;
        case TOK_INTERPOLATED_PATTERN:
            rv.m_data = new AST::Pattern( reinterpret_cast<AST::Pattern*>(e)->clone() );
            break;
        case TOK_INTERPOLATED_PATH:
            rv.m_data = new AST::Path( *reinterpret_cast<AST::Path*>(e) );
            break;
        case TOK_INTERPOLATED_EXPR:
            rv.m_data = reinterpret_cast<AST::ExprNode*>(e)->clone().release();
            break;
        case TOK_INTERPOLATED_STMT:
            rv.m_data = reinterpret_cast<AST::ExprNode*>(e)->clone().release();
            break;
        case TOK_INTERPOLATED_BLOCK:
            rv.m_data = reinterpret_cast<AST::ExprNode*>(e)->clone().release();
            break;
        case TOK_INTERPOLATED_META:
            rv.m_data = new AST::Attribute( reinterpret_cast<AST::Attribute*>(e)->clone() );
            break;
        case TOK_INTERPOLATED_ITEM:
            TODO(m_pos, "clone interpolated item");
            //rv.m_data = new AST::Named( AST::Item( reinterpret_cast<AST::Attribute*>(e)->clone() ) );
            break;
        default:
            BUG(m_pos, "Fragment with invalid token type (" << *this << ")");
            break;
        }
        assert(rv.m_data.is_Fragment());
        )
    )
    return rv;
}

::std::unique_ptr<AST::ExprNode> Token::take_frag_node()
{
    assert( m_type == TOK_INTERPOLATED_EXPR || m_type == TOK_INTERPOLATED_STMT || m_type == TOK_INTERPOLATED_BLOCK );
    auto ptr = m_data.as_Fragment();
    m_data.as_Fragment() = nullptr;
    return ::std::unique_ptr<AST::ExprNode>( reinterpret_cast<AST::ExprNode*>( ptr ) );
}
::AST::Named<AST::Item> Token::take_frag_item()
{
    assert( m_type == TOK_INTERPOLATED_ITEM );
    auto ptr = reinterpret_cast<AST::Named<AST::Item>*>(m_data.as_Fragment());
    m_data.as_Fragment() = nullptr;
    auto rv = mv$( *ptr );
    delete ptr;
    return mv$(rv);
}

const char* Token::typestr(enum eTokenType type)
{
    switch(type)
    {
    #define _(t)    case t: return #t;
    #include "eTokenType.enum.h"
    #undef _
    }
    return ">>BUGCHECK: BADTOK<<";
}

enum eTokenType Token::typefromstr(const ::std::string& s)
{
    if(s == "")
        return TOK_NULL;
    #define _(t)    if( s == #t ) return t;
    #include "eTokenType.enum.h"
    #undef _
    return TOK_NULL;
}

struct EscapedString {
    const ::std::string& s;
    EscapedString(const ::std::string& s): s(s) {}

    friend ::std::ostream& operator<<(::std::ostream& os, const EscapedString& x) {
        for(auto b : x.s) {
            switch(b)
            {
            case '"':
                os << "\\\"";
                break;
            case '\\':
                os << "\\\\";
                break;
            case '\n':
                os << "\\n";
                break;
            default:
                if( ' ' <= b && b < 0x7F )
                    os << b;
                else
                    os << "\\u{" << ::std::hex << (unsigned int)b << "}";
                break;
            }
        }
        return os;
    }
};

::std::string Token::to_str() const
{
    ::std::stringstream ss;
    switch(m_type)
    {
    case TOK_NULL:  return "/*null*/";
    case TOK_EOF:   return "/*eof*/";

    case TOK_NEWLINE:    return "\n";
    case TOK_WHITESPACE: return " ";
    case TOK_COMMENT:    return "/*" + m_data.as_String() + "*/";
    case TOK_INTERPOLATED_TYPE:
        reinterpret_cast<const ::TypeRef*>(m_data.as_Fragment())->print(ss, false);
        return ss.str();
    case TOK_INTERPOLATED_PATH:
        reinterpret_cast<const ::AST::Path*>(m_data.as_Fragment())->print_pretty(ss, true);
        return ss.str();
    case TOK_INTERPOLATED_PATTERN:
        // TODO: Use a pretty printer too?
        return FMT( *reinterpret_cast<const ::AST::Pattern*>(m_data.as_Fragment()) );
    case TOK_INTERPOLATED_STMT:
    case TOK_INTERPOLATED_BLOCK:
    case TOK_INTERPOLATED_EXPR: {
        ::std::stringstream ss;
        reinterpret_cast<const ::AST::ExprNode*>(m_data.as_Fragment())->print(ss);
        return ss.str();
        }
    case TOK_INTERPOLATED_META: return "/*:meta*/";
    case TOK_INTERPOLATED_ITEM: return "/*:item*/";
    case TOK_INTERPOLATED_IDENT: return "/*:ident*/";
    // Value tokens
    case TOK_IDENT:     return m_data.as_String();
    case TOK_LIFETIME:  return "'" + m_data.as_String();
    case TOK_INTEGER:
        if( m_data.as_Integer().m_datatype == CORETYPE_ANY ) {
            return FMT(m_data.as_Integer().m_intval);
        }
        else {
            return FMT(m_data.as_Integer().m_intval << "_" << m_data.as_Integer().m_datatype);
        }
    case TOK_CHAR:      return FMT("'\\u{"<< ::std::hex << m_data.as_Integer().m_intval << "}");
    case TOK_FLOAT:
        if( m_data.as_Float().m_datatype == CORETYPE_ANY ) {
            return FMT(m_data.as_Float().m_floatval);
        }
        else {
            return FMT(m_data.as_Float().m_floatval << "_" << m_data.as_Float().m_datatype);
        }
    case TOK_STRING:    return FMT("\"" << EscapedString(m_data.as_String()) << "\"");
    case TOK_BYTESTRING:return FMT("b\"" << m_data.as_String() << "\"");
    case TOK_HASH:  return "#";
    case TOK_UNDERSCORE:return "_";
    // Symbols
    case TOK_PAREN_OPEN:    return "(";
    case TOK_PAREN_CLOSE:   return ")";
    case TOK_BRACE_OPEN:    return "{";
    case TOK_BRACE_CLOSE:   return "}";
    case TOK_LT:    return "<";
    case TOK_GT:    return ">";
    case TOK_SQUARE_OPEN:   return "[";
    case TOK_SQUARE_CLOSE:  return "]";
    case TOK_COMMA:     return ",";
    case TOK_SEMICOLON: return ";";
    case TOK_COLON:     return ":";
    case TOK_DOUBLE_COLON:  return "::";
    case TOK_STAR:  return "*";
    case TOK_AMP:   return "&";
    case TOK_PIPE:  return "|";

    case TOK_FATARROW:  return "=>";
    case TOK_THINARROW: return "->";
    case TOK_THINARROW_LEFT: return "<-";

    case TOK_PLUS:  return "+";
    case TOK_DASH:  return "-";
    case TOK_EXCLAM:    return "!";
    case TOK_PERCENT:   return "%";
    case TOK_SLASH:     return "/";

    case TOK_DOT:   return ".";
    case TOK_DOUBLE_DOT:    return "...";
    case TOK_TRIPLE_DOT:    return "..";

    case TOK_EQUAL: return "=";
    case TOK_PLUS_EQUAL:    return "+=";
    case TOK_DASH_EQUAL:    return "-";
    case TOK_PERCENT_EQUAL: return "%=";
    case TOK_SLASH_EQUAL:   return "/=";
    case TOK_STAR_EQUAL:    return "*=";
    case TOK_AMP_EQUAL:     return "&=";
    case TOK_PIPE_EQUAL:    return "|=";

    case TOK_DOUBLE_EQUAL:  return "==";
    case TOK_EXCLAM_EQUAL:  return "!=";
    case TOK_GTE:    return ">=";
    case TOK_LTE:    return "<=";

    case TOK_DOUBLE_AMP:    return "&&";
    case TOK_DOUBLE_PIPE:   return "||";
    case TOK_DOUBLE_LT:     return "<<";
    case TOK_DOUBLE_GT:     return ">>";
    case TOK_DOUBLE_LT_EQUAL:   return "<=";
    case TOK_DOUBLE_GT_EQUAL:   return ">=";

    case TOK_DOLLAR:    return "$";

    case TOK_QMARK: return "?";
    case TOK_AT:    return "@";
    case TOK_TILDE:     return "~";
    case TOK_BACKSLASH: return "\\";
    case TOK_CARET:     return "^";
    case TOK_CARET_EQUAL:   return "^=";
    case TOK_BACKTICK:  return "`";

    // Reserved Words
    case TOK_RWORD_PUB:     return "pub";
    case TOK_RWORD_PRIV:    return "priv";
    case TOK_RWORD_MUT:     return "mut";
    case TOK_RWORD_CONST:   return "const";
    case TOK_RWORD_STATIC:  return "static";
    case TOK_RWORD_UNSAFE:  return "unsafe";
    case TOK_RWORD_EXTERN:  return "extern";

    case TOK_RWORD_CRATE:   return "crate";
    case TOK_RWORD_MOD:     return "mod";
    case TOK_RWORD_STRUCT:  return "struct";
    case TOK_RWORD_ENUM:    return "enum";
    case TOK_RWORD_TRAIT:   return "trait";
    case TOK_RWORD_FN:      return "fn";
    case TOK_RWORD_USE:     return "use";
    case TOK_RWORD_IMPL:    return "impl";
    case TOK_RWORD_TYPE:    return "type";

    case TOK_RWORD_WHERE:   return "where";
    case TOK_RWORD_AS:      return "as";

    case TOK_RWORD_LET:     return "let";
    case TOK_RWORD_MATCH:   return "match";
    case TOK_RWORD_IF:      return "if";
    case TOK_RWORD_ELSE:    return "else";
    case TOK_RWORD_LOOP:    return "loop";
    case TOK_RWORD_WHILE:   return "while";
    case TOK_RWORD_FOR:     return "for";
    case TOK_RWORD_IN:      return "in";
    case TOK_RWORD_DO:      return "do";

    case TOK_RWORD_CONTINUE:return "continue";
    case TOK_RWORD_BREAK:   return "break";
    case TOK_RWORD_RETURN:  return "return";
    case TOK_RWORD_YIELD:   return "yeild";
    case TOK_RWORD_BOX:     return "box";
    case TOK_RWORD_REF:     return "ref";

    case TOK_RWORD_FALSE:   return "false";
    case TOK_RWORD_TRUE:    return "true";
    case TOK_RWORD_SELF:    return "self";
    case TOK_RWORD_SUPER:   return "super";

    case TOK_RWORD_PROC:    return "proc";
    case TOK_RWORD_MOVE:    return "move";

    case TOK_RWORD_ABSTRACT:return "abstract";
    case TOK_RWORD_FINAL:   return "final";
    case TOK_RWORD_PURE:    return "pure";
    case TOK_RWORD_OVERRIDE:return "override";
    case TOK_RWORD_VIRTUAL: return "virtual";

    case TOK_RWORD_ALIGNOF: return "alignof";
    case TOK_RWORD_OFFSETOF:return "offsetof";
    case TOK_RWORD_SIZEOF:  return "sizeof";
    case TOK_RWORD_TYPEOF:  return "typeof";

    case TOK_RWORD_BE:      return "be";
    case TOK_RWORD_UNSIZED: return "unsized";
    }
    throw ParseError::BugCheck("Reached end of Token::to_str");
}


::std::ostream&  operator<<(::std::ostream& os, const Token& tok)
{
    os << Token::typestr(tok.type());
    switch(tok.type())
    {
    case TOK_STRING:
    case TOK_BYTESTRING:
    case TOK_IDENT:
    case TOK_LIFETIME:
        if( tok.m_data.is_String() )
            os << "\"" << EscapedString(tok.str()) << "\"";
        break;
    case TOK_INTEGER:
        if( tok.m_data.is_Integer() )
            os << ":" << tok.intval();
        break;
    case TOK_INTERPOLATED_TYPE:
        os << ":" << *reinterpret_cast<TypeRef*>(tok.m_data.as_Fragment());
        break;
    case TOK_INTERPOLATED_PATTERN:
        os << ":" << *reinterpret_cast<AST::Pattern*>(tok.m_data.as_Fragment());
        break;
    case TOK_INTERPOLATED_PATH:
        os << ":" << *reinterpret_cast<AST::Path*>(tok.m_data.as_Fragment());
        break;
    case TOK_INTERPOLATED_EXPR:
        os << ":" << *reinterpret_cast<AST::ExprNode*>(tok.m_data.as_Fragment());
        break;
    case TOK_INTERPOLATED_STMT:
        os << ":" << *reinterpret_cast<AST::ExprNode*>(tok.m_data.as_Fragment());
        break;
    case TOK_INTERPOLATED_BLOCK:
        os << ":" << *reinterpret_cast<AST::ExprNode*>(tok.m_data.as_Fragment());
        break;
    case TOK_INTERPOLATED_META:
        os << ":" << *reinterpret_cast<AST::Attribute*>(tok.m_data.as_Fragment());
        break;
    case TOK_INTERPOLATED_ITEM: {
        const auto& named_item = *reinterpret_cast<const AST::Named<AST::Item>*>(tok.m_data.as_Fragment());
        os << ":" << named_item.data.tag_str() << "(" << named_item.name << ")";
        } break;
    default:
        break;
    }
    return os;
}
::std::ostream& operator<<(::std::ostream& os, const Position& p)
{
    return os << ::std::dec << p.filename << ":" << p.line;
}

