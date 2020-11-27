/*
 * MRustC - Mutabah's Rust Compiler
 * - By John Hodge (Mutabah/thePowersGang)
 *
 * types.cpp
 * - Backing code for the TypeRef class
 *
 * Handles a chunk of type resolution (merging) and matching/comparing types
 */
#include "types.hpp"
#include "ast/ast.hpp"
#include <ast/crate.hpp>
#include <ast/expr.hpp>

/// Mappings from internal type names to the core type enum
static const struct {
    const char* name;
    enum eCoreType  type;
} CORETYPES[] = {
    // NOTE: Sorted
    {"_", CORETYPE_ANY},
    {"bool", CORETYPE_BOOL},
    {"char", CORETYPE_CHAR},
    {"f32", CORETYPE_F32},
    {"f64", CORETYPE_F64},
    {"i128", CORETYPE_I128},
    {"i16", CORETYPE_I16},
    {"i32", CORETYPE_I32},
    {"i64", CORETYPE_I64},
    {"i8", CORETYPE_I8},
    {"int", CORETYPE_INT},
    {"isize", CORETYPE_INT},
    {"str", CORETYPE_STR},
    {"u128", CORETYPE_U128},
    {"u16", CORETYPE_U16},
    {"u32", CORETYPE_U32},
    {"u64", CORETYPE_U64},
    {"u8",  CORETYPE_U8},
    {"uint", CORETYPE_UINT},
    {"usize", CORETYPE_UINT},
};

enum eCoreType coretype_fromstring(const ::std::string& name)
{
    for(unsigned int i = 0; i < sizeof(CORETYPES)/sizeof(CORETYPES[0]); i ++)
    {
        if( name < CORETYPES[i].name )
            break;
        if( name == CORETYPES[i].name )
            return CORETYPES[i].type;
    }
    return CORETYPE_INVAL;
}

const char* coretype_name(const eCoreType ct ) {
    switch(ct)
    {
    case CORETYPE_INVAL:return "INVAL";
    case CORETYPE_ANY:  return "_";
    case CORETYPE_CHAR: return "char";
    case CORETYPE_STR:  return "str";
    case CORETYPE_BOOL: return "bool";
    case CORETYPE_UINT: return "usize";
    case CORETYPE_INT:  return "isize";
    case CORETYPE_U8:   return "u8";
    case CORETYPE_I8:   return "i8";
    case CORETYPE_U16:  return "u16";
    case CORETYPE_I16:  return "i16";
    case CORETYPE_U32:  return "u32";
    case CORETYPE_I32:  return "i32";
    case CORETYPE_U64:  return "u64";
    case CORETYPE_I64:  return "i64";
    case CORETYPE_U128: return "u128";
    case CORETYPE_I128: return "i128";
    case CORETYPE_F32:  return "f32";
    case CORETYPE_F64:  return "f64";
    }
    DEBUG("Unknown core type?! " << ct);
    return "NFI";
}

Type_Function::Type_Function(const Type_Function& other):
    is_unsafe(other.is_unsafe),
    m_abi(other.m_abi),
    m_rettype( box$( other.m_rettype->clone() ) )
{
    for( const auto& at : other.m_arg_types )
        m_arg_types.push_back( at.clone() );
}

Ordering Type_Function::ord(const Type_Function& x) const
{
    Ordering rv;

    rv = ::ord(m_abi, x.m_abi);
    if(rv != OrdEqual)  return rv;
    rv = ::ord(m_arg_types, x.m_arg_types);
    if(rv != OrdEqual)  return rv;
    return (*m_rettype).ord( *x.m_rettype );
}

TypeRef::~TypeRef()
{
}

TypeRef TypeRef::clone() const
{
    struct H {
        static ::std::vector< ::TypeRef> clone_ty_vec(const ::std::vector<TypeRef>& x) {
            ::std::vector<TypeRef>  rv;
            rv.reserve(x.size());
            for(const auto& t : x)
                rv.push_back( t.clone() );
            return rv;
        }
    };
    switch( m_data.tag() )
    {
    case TypeData::TAGDEAD: assert(!"Copying a destructed type");
    #define _COPY(VAR)  case TypeData::TAG_##VAR: return TypeRef(m_span, TypeData::make_##VAR(m_data.as_##VAR()) ); break;
    #define _CLONE(VAR, ...)    case TypeData::TAG_##VAR: { auto& old = m_data.as_##VAR(); return TypeRef(m_span, TypeData::make_##VAR(__VA_ARGS__) ); } break;
    _COPY(None)
    _COPY(Any)
    _COPY(Bang)
    _CLONE(Macro, { old.inv.clone() })
    //case TypeData::TAG_Macro:   assert( !"Copying an unexpanded type macro" );
    _COPY(Unit)
    _COPY(Primitive)
    _COPY(Function)
    _CLONE(Tuple, { H::clone_ty_vec(old.inner_types) })
    _CLONE(Borrow,  { AST::LifetimeRef(old.lifetime), old.is_mut, box$(old.inner->clone()) })
    _CLONE(Pointer, { old.is_mut, box$(old.inner->clone()) })
    _CLONE(Array, { box$(old.inner->clone()), old.size })
    _COPY(Generic)
    _COPY(Path)
    _COPY(TraitObject)
    _COPY(ErasedType)
    #undef _COPY
    #undef _CLONE
    }
    throw "";
}

Ordering Type_TraitPath::ord(const Type_TraitPath& x) const
{
    Ordering    rv;

    rv = ::ord( this->path, x.path );
    if(rv != OrdEqual)  return rv;

    return rv;
}
Ordering TypeRef::ord(const TypeRef& x) const
{
    Ordering    rv;

    rv = ::ord( (unsigned)m_data.tag(), (unsigned)x.m_data.tag() );
    if(rv != OrdEqual)  return rv;

    TU_MATCH(TypeData, (m_data, x.m_data), (ent, x_ent),
    (None, return OrdEqual;),
    (Macro, throw CompileError::BugCheck("TypeRef::ord - unexpanded macro");),
    (Any,  return OrdEqual;),
    (Unit, return OrdEqual;),
    (Bang, return OrdEqual;),
    (Primitive,
        return ::ord( (unsigned)ent.core_type, (unsigned)x_ent.core_type );
        ),
    (Function,
        return ent.info.ord( x_ent.info );
        ),
    (Tuple,
        return ::ord(ent.inner_types, x_ent.inner_types);
        ),
    (Borrow,
        rv = ::ord(ent.is_mut, x_ent.is_mut);
        if(rv != OrdEqual)  return rv;
        return (*ent.inner).ord(*x_ent.inner);
        ),
    (Pointer,
        rv = ::ord(ent.is_mut, x_ent.is_mut);
        if(rv != OrdEqual)  return rv;
        return (*ent.inner).ord(*x_ent.inner);
        ),
    (Array,
        rv = (*ent.inner).ord( *x_ent.inner );
        if(rv != OrdEqual)  return rv;
        if(ent.size.get())
        {
            throw ::std::runtime_error("TODO: Sized array comparisons");
        }
        return OrdEqual;
        ),
    (Generic,
        return ::ord(ent.name, x_ent.name);
        ),
    (Path,
        return ent.path.ord( x_ent.path );
        ),
    (TraitObject,
        return ::ord(ent.traits, x_ent.traits);
        ),
    (ErasedType,
        return ::ord(ent.traits, x_ent.traits);
        )
    )
    throw ::std::runtime_error(FMT("BUGCHECK - Unhandled TypeRef class '" << m_data.tag() << "'"));
}

::std::ostream& operator<<(::std::ostream& os, const eCoreType ct) {
    return os << coretype_name(ct);
}

void TypeRef::print(::std::ostream& os, bool is_debug/*=false*/) const
{
    //os << "TypeRef(";
    #define _(VAR, ...) case TypeData::TAG_##VAR: { const auto &ent = this->m_data.as_##VAR(); (void)&ent; __VA_ARGS__ } break;
    switch(this->m_data.tag())
    {
    case TypeData::TAGDEAD: throw "";
    _(None,
        os << "!!";
        )
    _(Any,
        os << "_";
        )
    _(Bang,
        os << "!";
        )
    _(Macro,
        os << ent.inv;
        )
    _(Unit,
        os << "()";
        )
    _(Primitive,
        os << ent.core_type;
        )
    _(Function,
        if( ent.info.m_abi != "" )
            os << "extern \"" << ent.info.m_abi << "\" ";
        os << "fn (";
        for( const auto& arg : ent.info.m_arg_types )
        {
            arg.print(os, is_debug);
            os << ", ";
        }
        os << ") -> " << *ent.info.m_rettype;
        )
    _(Tuple,
        os << "( ";
        for( const auto& it : ent.inner_types )
        {
            it.print(os, is_debug);
            os << ", ";
        }
        os << ")";
        )
    _(Borrow,
        os << "&" << (ent.is_mut ? "mut " : "");
        ent.inner->print(os, is_debug);
        )
    _(Pointer,
        os << "*" << (ent.is_mut ? "mut" : "const");
        ent.inner->print(os, is_debug);
        )
    _(Array,
        os << "[";
        ent.inner->print(os, is_debug);
        if( ent.size.get() )
            os << "; " << *ent.size;
        os << "]";
        )
    _(Generic,
        if(is_debug)
            os << "/* arg */ ";
        os << ent.name;
        if(is_debug)
            os << "/*"<<ent.index<<"*/";
        )
    _(Path,
        ent.path.print_pretty(os, true, is_debug);
        )
    _(TraitObject,
        os << "(";
        for( const auto& it : ent.traits ) {
            if( &it != &ent.traits.front() )
                os << "+";
            os << it.hrbs;
            it.path.print_pretty(os, true, is_debug);
        }
        os << ")";
        )
    _(ErasedType,
        os << "impl ";
        for( const auto& it : ent.traits ) {
            if( &it != &ent.traits.front() )
                os << "+";
            os << it.hrbs;
            it.path.print_pretty(os, true, is_debug);
        }
        os << "";
        )
    }
    #undef _
}

::std::ostream& operator<<(::std::ostream& os, const TypeRef& tr) {
    tr.print(os, true);
    return os;
}
::std::ostream& operator<<(::std::ostream& os, const PrettyPrintType& x) {
    x.m_type.print(os, false);
    return os;
}

namespace AST {
    ::std::ostream& operator<<(::std::ostream& os, const LifetimeRef& x) {
        if( x.m_binding == LifetimeRef::BINDING_STATIC ) {
            os << "'static";
        }
        else if( x.m_binding == LifetimeRef::BINDING_INFER ) {
            os << "'_";
        }
        else {
            os << "'" << x.m_name;
            if( x.m_binding != LifetimeRef::BINDING_UNBOUND ) {
                os << "/*" << x.m_binding << "*/";
            }
        }
        return os;
    }
}

