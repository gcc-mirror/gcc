/*
 * MRustC - Mutabah's Rust Compiler
 * - By John Hodge (Mutabah/thePowersGang)
 *
 * ast/path.cpp
 * - AST::Path and friends
 */
#include "path.hpp"
#include "ast.hpp"
#include "types.hpp"
#include <iostream>
#include "../parse/parseerror.hpp"
#include <algorithm>

#define PRETTY_PATH_PRINT   1

namespace AST {

// --- AST::PathBinding
::std::ostream& operator<<(::std::ostream& os, const PathBinding& x) {
    TU_MATCH(PathBinding, (x), (i),
    (Unbound, os << "_";   ),
    (Crate ,  os << "Crate";    ),
    (Module,  os << "Module";    ),
    (Trait,     os << "Trait";   ),
    (Struct,    os << "Struct";  ),
    (Enum,      os << "Enum";    ),
    (Union,     os << "Union";   ),
    (Static,    os << "Static";  ),
    (Function,  os << "Function";),
    (EnumVar,  os << "EnumVar(" << i.idx << ")"; ),
    (TypeAlias, os << "TypeAlias";),
    (StructMethod, os << "StructMethod"; ),
    (TraitMethod,  os << "TraitMethod";  ),

    (TypeParameter, os << "TyParam(" << i.level << " # " << i.idx << ")"; ),
    (Variable, os << "Var(" << i.slot << ")"; )
    )
    return os;
}
PathBinding PathBinding::clone() const
{
    TU_MATCH(::AST::PathBinding, (*this), (e),
    (Unbound , return PathBinding::make_Unbound({}); ),
    (Module  , return PathBinding::make_Module(e);   ),
    (Crate   , return PathBinding(e); ),
    (Trait   , return PathBinding(e); ),
    (Struct  , return PathBinding(e); ),
    (Enum    , return PathBinding(e); ),
    (Union   , return PathBinding(e); ),
    (Static  , return PathBinding(e); ),
    (Function, return PathBinding(e); ),
    (TypeAlias, return PathBinding::make_TypeAlias(e); ),
    (EnumVar , return PathBinding::make_EnumVar(e);  ),
    (StructMethod, return PathBinding::make_StructMethod(e); ),
    (TraitMethod, return PathBinding::make_TraitMethod(e); ),

    (TypeParameter, return PathBinding::make_TypeParameter(e); ),
    (Variable, return PathBinding::make_Variable(e); )
    )
    throw "BUG: Fell off the end of PathBinding::clone";
}

::std::ostream& operator<<(::std::ostream& os, const PathParams& x)
{
    bool needs_comma = false;
    os << "<";
    for(const auto& v : x.m_lifetimes) {
        if(needs_comma) os << ", ";
        needs_comma = true;
        os << "'" << v;
    }
    for(const auto& v : x.m_types) {
        if(needs_comma) os << ", ";
        needs_comma = true;
        os << v;
    }
    for(const auto& v : x.m_assoc) {
        if(needs_comma) os << ", ";
        needs_comma = true;
        os << v.first << "=" << v.second;
    }
    os << ">";
    return os;
}
PathParams::PathParams(const PathParams& x):
    m_lifetimes( x.m_lifetimes )
{
    m_types.reserve( x.m_types.size() );
    for(const auto& t : x.m_types)
        m_types.push_back(t.clone());

    m_assoc.reserve( x.m_assoc.size() );
    for(const auto& t : x.m_assoc)
        m_assoc.push_back( ::std::make_pair(t.first, t.second.clone()) );
}
Ordering PathParams::ord(const PathParams& x) const
{
    Ordering rv;
    rv = ::ord(m_lifetimes, x.m_lifetimes);
    if(rv != OrdEqual)  return rv;
    rv = ::ord(m_types, x.m_types);
    if(rv != OrdEqual)  return rv;
    rv = ::ord(m_assoc, x.m_assoc);
    if(rv != OrdEqual)  return rv;
    return rv;
}

// --- AST::PathNode
PathNode::PathNode(::std::string name, PathParams args):
    m_name( mv$(name) ),
    m_params( mv$(args) )
{
}
Ordering PathNode::ord(const PathNode& x) const
{
    Ordering    rv;
    rv = ::ord(m_name, x.m_name);
    if(rv != OrdEqual)  return rv;
    rv = m_params.ord(x.m_params);
    if(rv != OrdEqual)  return rv;
    return OrdEqual;
}
void PathNode::print_pretty(::std::ostream& os, bool is_type_context) const
{
    os << m_name;
    if( ! m_params.is_empty() )
    {
        if( ! is_type_context )
            os << "::";
        os << m_params;
    }
}
::std::ostream& operator<<(::std::ostream& os, const PathNode& pn) {
    pn.print_pretty(os, false);
    return os;
}

/// Return an iterator to the named item
template<typename T>
typename ::std::vector<Named<T> >::const_iterator find_named(const ::std::vector<Named<T> >& vec, const ::std::string& name)
{
    return ::std::find_if(vec.begin(), vec.end(), [&name](const Named<T>& x) {
        return x.name == name;
    });
}

// --- AST::Path
AST::Path::~Path()
{
}
AST::Path::Path(TagUfcs, TypeRef type, ::std::vector<AST::PathNode> nodes):
    m_class( AST::Path::Class::make_UFCS({box$(type), nullptr, nodes}) )
{
}
AST::Path::Path(TagUfcs, TypeRef type, Path trait, ::std::vector<AST::PathNode> nodes):
    m_class( AST::Path::Class::make_UFCS({box$(type), box$(trait), nodes}) )
{
}
AST::Path::Path(const Path& x):
    m_class()
    //m_binding(x.m_binding)
{
    TU_MATCH(Class, (x.m_class), (ent),
    (Invalid, m_class = Class::make_Invalid({});),
    (Local,
        m_class = Class::make_Local({ent.name});
        ),
    (Relative,
        m_class = Class::make_Relative({ent.hygiene, ent.nodes});
        ),
    (Self,
        m_class = Class::make_Self({ent.nodes});
        ),
    (Super,
        m_class = Class::make_Super({ent.count, ent.nodes});
        ),
    (Absolute,
        m_class = Class::make_Absolute({ent.crate, ent.nodes});
        ),
    (UFCS,
        if( ent.trait )
            m_class = Class::make_UFCS({ box$(ent.type->clone()), ::std::unique_ptr<Path>(new Path(*ent.trait)), ent.nodes });
        else
            m_class = Class::make_UFCS({ box$(ent.type->clone()), nullptr, ent.nodes });
        )
    )

    memcpy(&m_binding, &x.m_binding, sizeof(PathBinding));
}

void Path::bind_variable(unsigned int slot)
{
    m_binding = PathBinding::make_Variable({slot});
}
void Path::bind_enum_var(const Enum& ent, const ::std::string& name, const ::std::vector<TypeRef>& /*args*/)
{
    auto it = ::std::find_if(ent.variants().begin(), ent.variants().end(), [&](const auto& x) { return x.m_name == name; });
    if( it == ent.variants().end() )
    {
        throw ParseError::Generic("Enum variant not found");
    }
    unsigned int idx = it - ent.variants().begin();

    DEBUG("Bound to enum variant '" << name << "' (#" << idx << ")");
    ::AST::PathBinding::Data_EnumVar tmp = {};
    tmp.enum_ = &ent;
    tmp.idx = idx;
    m_binding = PathBinding::make_EnumVar( mv$(tmp) );
}

Path& Path::operator+=(const Path& other)
{
    for(auto& node : other.nodes())
        append(node);
    // If the path is modified, clear the binding
    m_binding = PathBinding();
    return *this;
}

Ordering Path::ord(const Path& x) const
{
    Ordering rv;

    rv = ::ord( (unsigned)m_class.tag(), (unsigned)x.m_class.tag() );
    if( rv != OrdEqual )    return rv;

    TU_MATCH(Path::Class, (m_class, x.m_class), (ent, x_ent),
    (Invalid,
        return OrdEqual;
        ),
    (Local,
        return ::ord(ent.name, x_ent.name);
        ),
    (Relative,
        return ::ord(ent.nodes, x_ent.nodes);
        ),
    (Self,
        return ::ord(ent.nodes, x_ent.nodes);
        ),
    (Super,
        return ::ord(ent.nodes, x_ent.nodes);
        ),
    (Absolute,
        rv = ::ord( ent.crate, x_ent.crate );
        if( rv != OrdEqual )    return rv;
        return ::ord(ent.nodes, x_ent.nodes);
        ),
    (UFCS,
        rv = ent.type->ord( *x_ent.type );
        if( rv != OrdEqual )    return rv;
        rv = ent.trait->ord( *x_ent.trait );
        if( rv != OrdEqual )    return rv;
        return ::ord(ent.nodes, x_ent.nodes);
        )
    )

    return OrdEqual;
}

void Path::print_pretty(::std::ostream& os, bool is_type_context, bool is_debug) const
{
    TU_MATCH(Path::Class, (m_class), (ent),
    (Invalid,
        os << "/*inv*/";
        // NOTE: Don't print the binding for invalid paths
        return ;
        ),
    (Local,
        // Only print comment if there's no binding
        if( m_binding.is_Unbound() )
        {
            if( is_debug )
                os << "/*var*/";
        }
        else
            assert( m_binding.is_Variable() );
        os << ent.name;
        ),
    (Relative,
        if( is_debug )
            os << ent.hygiene;
        for(const auto& n : ent.nodes)
        {
            if( &n != &ent.nodes[0] ) {
                os << "::";
            }
            n.print_pretty(os, is_type_context);
        }
        ),
    (Self,
        os << "self";
        for(const auto& n : ent.nodes)
        {
            os << "::";
            n.print_pretty(os, is_type_context);
        }
        ),
    (Super,
        os << "super";
        for(const auto& n : ent.nodes)
        {
            os << "::";
            n.print_pretty(os, is_type_context);
        }
        ),
    (Absolute,
        if( ent.crate != "" )
            os << "::\"" << ent.crate << "\"";
        for(const auto& n : ent.nodes)
        {
            os << "::";
            n.print_pretty(os, is_type_context);
        }
        ),
    (UFCS,
        //os << "/*ufcs*/";
        if( ent.trait ) {
            os << "<" << *ent.type << " as ";
            if( ent.trait->m_class.is_Invalid() ) {
                os << "_";
            }
            else {
                os << *ent.trait;
            }
            os << ">";
        }
        else {
            os << "<" << *ent.type << ">";
        }
        for(const auto& n : ent.nodes) {
            os << "::";
            n.print_pretty(os, is_type_context);
        }
        )
    )
    if( is_debug )
        os << "/*" << m_binding << "*/";
}

::std::ostream& operator<<(::std::ostream& os, const Path& path)
{
    path.print_pretty(os, false, true);
    return os;
}

}
