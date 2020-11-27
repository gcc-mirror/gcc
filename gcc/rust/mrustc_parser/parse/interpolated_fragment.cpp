/*
 * MRustC - Mutabah's Rust Compiler
 * - By John Hodge (Mutabah/thePowersGang)
 *
 * parse/interpolated_fragment.cpp
 * - An "interpolated fragment", result of parsing e.g. :expr in a macro invocation
 */
#include <iostream>
#include "interpolated_fragment.hpp"
#include <ast/ast.hpp>
#include <ast/expr.hpp> // For definition of ExprNode

InterpolatedFragment::~InterpolatedFragment()
{
    if( m_ptr )
    {
        switch(m_type)
        {
        case InterpolatedFragment::TT:  delete reinterpret_cast<TokenTree*>(m_ptr);  break;
        case InterpolatedFragment::PAT: delete reinterpret_cast<AST::Pattern*>(m_ptr); break;
        case InterpolatedFragment::PATH:delete reinterpret_cast<AST::Path*>(m_ptr);    break;
        case InterpolatedFragment::TYPE:delete reinterpret_cast<TypeRef*>(m_ptr);    break;
        case InterpolatedFragment::EXPR:
        case InterpolatedFragment::STMT:
        case InterpolatedFragment::BLOCK:
            delete reinterpret_cast<AST::ExprNode*>(m_ptr);
            break;
        case InterpolatedFragment::META:
            delete reinterpret_cast<AST::Attribute*>(m_ptr);
            break;
        case InterpolatedFragment::ITEM:
            delete reinterpret_cast<AST::Named<AST::Item>*>(m_ptr);
            break;
        }
    }
}

InterpolatedFragment::InterpolatedFragment(InterpolatedFragment&& x):
    m_type( x.m_type )
{
    m_ptr = x.m_ptr, x.m_ptr = nullptr;
}
InterpolatedFragment& InterpolatedFragment::operator=(InterpolatedFragment&& x)
{
    m_type = x.m_type;
    m_ptr = x.m_ptr, x.m_ptr = nullptr;
    return *this;
}

InterpolatedFragment::InterpolatedFragment(InterpolatedFragment::Type type, AST::ExprNode* ptr):
    m_type( type ),
    m_ptr( ptr )
{
}
InterpolatedFragment::InterpolatedFragment(AST::Attribute v):
    m_type( InterpolatedFragment::META ),
    m_ptr( new AST::Attribute(mv$(v)) )
{
}
InterpolatedFragment::InterpolatedFragment(::AST::Named<::AST::Item> v):
    m_type( InterpolatedFragment::ITEM ),
    m_ptr( new ::AST::Named<::AST::Item>( mv$(v) ) )
{
}
InterpolatedFragment::InterpolatedFragment(TokenTree v):
    m_type( InterpolatedFragment::TT ),
    m_ptr( new TokenTree(mv$(v)) )
{
}
InterpolatedFragment::InterpolatedFragment(AST::Path v):
    m_type( InterpolatedFragment::PATH ),
    m_ptr( new AST::Path(mv$(v)) )
{
}
InterpolatedFragment::InterpolatedFragment(AST::Pattern v):
    m_type( InterpolatedFragment::PAT ),
    m_ptr( new AST::Pattern(mv$(v)) )
{
}
InterpolatedFragment::InterpolatedFragment(TypeRef v):
    m_type( InterpolatedFragment::TYPE ),
    m_ptr( new TypeRef(mv$(v)) )
{
}

::std::ostream& operator<<(::std::ostream& os, InterpolatedFragment const& x)
{
    switch(x.m_type)
    {
    case InterpolatedFragment::TT:
        os << "tt[" << x.as_tt() << "]";
        break;
    case InterpolatedFragment::PAT:
        os << "pat[" << *reinterpret_cast<AST::Pattern*>(x.m_ptr) << "]";
        break;
    case InterpolatedFragment::PATH:
        os << "path[" << *reinterpret_cast<AST::Path*>(x.m_ptr) << "]";
        break;
    case InterpolatedFragment::TYPE:
        os << "type[" << *reinterpret_cast<TypeRef*>(x.m_ptr) << "]";
        break;

    case InterpolatedFragment::EXPR:
        os << "expr[" << *reinterpret_cast<const AST::ExprNode*>(x.m_ptr) << "]";
        break;
    case InterpolatedFragment::STMT:
        os << "stmt[" << *reinterpret_cast<const AST::ExprNode*>(x.m_ptr) << "]";
        break;
    case InterpolatedFragment::BLOCK:
        os << "block[" << *reinterpret_cast<const AST::ExprNode*>(x.m_ptr) << "]";
        break;

    case InterpolatedFragment::META:
        os << "meta[" << *reinterpret_cast<const AST::Attribute*>(x.m_ptr) << "]";
        break;
    case InterpolatedFragment::ITEM: {
        const auto& named_item = *reinterpret_cast<const AST::Named<AST::Item>*>(x.m_ptr);
        os << "item[" << named_item.data.tag_str() << "(" << named_item.name << ")]";
        } break;
    }
    return os;
}

