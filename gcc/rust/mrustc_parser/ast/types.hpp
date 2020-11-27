/*
 * MRustC - Mutabah's Rust Compiler
 * - By John Hodge (Mutabah/thePowersGang)
 *
 * ast/types.hpp
 * - AST Type reference (and helpers)
 */
#ifndef TYPES_HPP_INCLUDED
#define TYPES_HPP_INCLUDED

#include <memory>

#include "../common.hpp"
#include "coretypes.hpp"
#include "ast/path.hpp"
#include "ast/macro.hpp"
#include <tagged_union.hpp>

namespace AST {
class ExprNode;
class Expr;
class LifetimeParam;
}

namespace AST {

    // Defined here for dependency reasons
    class HigherRankedBounds
    {
    public:
        ::std::vector<LifetimeParam>    m_lifetimes;
        //::std::vector<TypeParam>    m_types;
        //::std::vector<GenericBound>    m_bounds;

        bool empty() const {
            return m_lifetimes.empty();
        }

        friend ::std::ostream& operator<<(::std::ostream& os, const HigherRankedBounds& x);
    };

    class LifetimeRef
    {
        static const uint16_t BINDING_STATIC = 0xFFFF;
        static const uint16_t BINDING_UNBOUND = 0xFFFE;
        static const uint16_t BINDING_INFER = 0xFFFD;

        Ident   m_name;
        uint16_t  m_binding;

        LifetimeRef(Ident name, uint32_t binding):
            m_name( ::std::move(name) ),
            m_binding( binding )
        {
        }
    public:
        LifetimeRef():
            LifetimeRef("", BINDING_INFER)
        {
        }
        LifetimeRef(Ident name):
            LifetimeRef(::std::move(name), BINDING_UNBOUND)
        {
        }
        static LifetimeRef new_static() {
            return LifetimeRef("static", BINDING_STATIC);
        }

        void set_binding(uint16_t b) { assert(m_binding == BINDING_UNBOUND); m_binding = b; }
        bool is_unbound() const { return m_binding == BINDING_UNBOUND; }
        bool is_infer() const { return m_binding == BINDING_INFER; }

        const Ident& name() const { return m_name; }
        Ordering ord(const LifetimeRef& x) const { return ::ord(m_name.name, x.m_name.name); }
        bool operator==(const LifetimeRef& x) const { return ord(x) == OrdEqual; }
        bool operator!=(const LifetimeRef& x) const { return ord(x) != OrdEqual; }
        bool operator<(const LifetimeRef& x) const { return ord(x) == OrdLess; };

        friend ::std::ostream& operator<<(::std::ostream& os, const LifetimeRef& x);
    };
}

class PrettyPrintType
{
    const TypeRef&  m_type;
public:
    PrettyPrintType(const TypeRef& ty):
        m_type(ty)
    {}

    void print(::std::ostream& os) const;

    friend ::std::ostream& operator<<(::std::ostream& os, const PrettyPrintType& v);
};

struct TypeArgRef
{
    ::std::string   name;
    unsigned int    level;
    const AST::GenericParams*  params;
};

struct Type_Function
{
    AST::HigherRankedBounds hrbs;
    bool    is_unsafe;
    ::std::string   m_abi;
    ::std::unique_ptr<TypeRef>  m_rettype;
    ::std::vector<TypeRef>  m_arg_types;
    bool is_variadic;

    Type_Function() {}
    Type_Function(AST::HigherRankedBounds hrbs, bool is_unsafe, ::std::string abi, ::std::unique_ptr<TypeRef> ret, ::std::vector<TypeRef> args, bool is_variadic):
        hrbs(mv$(hrbs)),
        is_unsafe(is_unsafe),
        m_abi(mv$(abi)),
        m_rettype(mv$(ret)),
        m_arg_types(mv$(args)),
        is_variadic(is_variadic)
    {}
    Type_Function(Type_Function&& other) = default;
    Type_Function(const Type_Function& other);

    Ordering ord(const Type_Function& x) const;
};

struct Type_TraitPath
{
    AST::HigherRankedBounds hrbs;
    AST::Path   path;

    Ordering ord(const Type_TraitPath& x) const;
};

TAGGED_UNION(TypeData, None,
    (None, struct { }),
    (Any,  struct { }),
    (Bang, struct { }),
    (Unit, struct { }),
    (Macro, struct {
        ::AST::MacroInvocation inv;
        }),
    (Primitive, struct {
        enum eCoreType core_type;
        }),
    (Function, struct {
        Type_Function   info;
        }),
    (Tuple, struct {
        ::std::vector<TypeRef> inner_types;
        }),
    (Borrow, struct {
        AST::LifetimeRef lifetime;
        bool is_mut;
        ::std::unique_ptr<TypeRef> inner;
        }),
    (Pointer, struct {
        bool is_mut;
        ::std::unique_ptr<TypeRef> inner;
        }),
    (Array, struct {
        ::std::unique_ptr<TypeRef> inner;
        ::std::shared_ptr<AST::ExprNode> size;
        }),
    (Generic, struct {
        ::std::string name;
        unsigned int index;
        }),
    (Path, struct {
        AST::Path path;
        }),
    (TraitObject, struct {
        ::std::vector<Type_TraitPath>   traits;
        ::std::vector<AST::LifetimeRef> lifetimes;
        }),
    (ErasedType, struct {
        ::std::vector<Type_TraitPath>   traits;
        ::std::vector<AST::LifetimeRef> lifetimes;
        })
    );

/// A type
class TypeRef
{
    Span    m_span;
public:
    TypeData    m_data;

    ~TypeRef();

    TypeRef(TypeRef&& other) = default;
    TypeRef& operator=(TypeRef&& other) = default;

    #if 1
    TypeRef(const TypeRef& other) = delete;
    TypeRef& operator=(const TypeRef& other) = delete;
    #else
    TypeRef(const TypeRef& other): m_span(other.m_span) {
        *this = other.clone();
    }
    TypeRef& operator=(const TypeRef& other) {
        m_data = mv$(other.clone().m_data);
        return *this;
    }
    #endif

    TypeRef(Span sp):
        m_span( mv$(sp) ),
        m_data( TypeData::make_Any({}) )
    {}
    TypeRef(Span sp, TypeData data):
        m_span( mv$(sp) ),
        m_data( mv$(data) )
    {}

    struct TagInvalid {};
    TypeRef(TagInvalid, Span sp):
        m_span(mv$(sp)),
        m_data(TypeData::make_None({}))
    {}

    struct TagMacro {};
    TypeRef(TagMacro, ::AST::MacroInvocation inv):
        m_span(inv.span()),
        m_data(TypeData::make_Macro({mv$(inv)}))
    {}

    struct TagUnit {};  // unit maps to a zero-length tuple, just easier to type
    TypeRef(TagUnit, Span sp):
        m_span(mv$(sp)),
        m_data(TypeData::make_Unit({}))
    {}

    struct TagPrimitive {};
    TypeRef(TagPrimitive, Span sp, enum eCoreType type):
        m_span(mv$(sp)),
        m_data(TypeData::make_Primitive({type}))
    {}
    TypeRef(Span sp, enum eCoreType type):
        m_span(mv$(sp)),
        m_data(TypeData::make_Primitive({type}))
    {}

    struct TagTuple {};
    TypeRef(TagTuple , Span sp, ::std::vector<TypeRef> inner_types):
        m_span(mv$(sp)),
        m_data(TypeData::make_Tuple({::std::move(inner_types)}))
    {}
    struct TagFunction {};
    TypeRef(TagFunction, Span sp, AST::HigherRankedBounds hrbs, bool is_unsafe, ::std::string abi, ::std::vector<TypeRef> args, bool is_variadic, TypeRef ret):
        m_span(mv$(sp)),
        m_data(TypeData::make_Function({ Type_Function( mv$(hrbs), is_unsafe, abi, box$(ret), mv$(args), is_variadic ) }))
    {}

    struct TagReference {};
    TypeRef(TagReference , Span sp, AST::LifetimeRef lft, bool is_mut, TypeRef inner_type):
        m_span(mv$(sp)),
        m_data(TypeData::make_Borrow({ ::std::move(lft), is_mut, ::make_unique_ptr(mv$(inner_type)) }))
    {}
    struct TagPointer {};
    TypeRef(TagPointer , Span sp, bool is_mut, TypeRef inner_type):
        m_span(mv$(sp)),
        m_data(TypeData::make_Pointer({ is_mut, ::make_unique_ptr(mv$(inner_type)) }))
    {}
    struct TagSizedArray {};
    TypeRef(TagSizedArray , Span sp, TypeRef inner_type, ::std::shared_ptr<AST::ExprNode> size):
        m_span(mv$(sp)),
        m_data(TypeData::make_Array({ ::make_unique_ptr(mv$(inner_type)), mv$(size) }))
    {}
    struct TagUnsizedArray {};
    TypeRef(TagUnsizedArray , Span sp, TypeRef inner_type):
        m_span(mv$(sp)),
        m_data(TypeData::make_Array({ ::make_unique_ptr(mv$(inner_type)), ::std::shared_ptr<AST::ExprNode>() }))
    {}

    struct TagArg {};
    TypeRef(TagArg, Span sp, ::std::string name, unsigned int binding = ~0u):
        m_span( mv$(sp) ),
        m_data(TypeData::make_Generic({ name, binding }))
    {}
    TypeRef(Span sp, ::std::string name, unsigned int binding = ~0u):
        TypeRef(TagArg(), mv$(sp), mv$(name), binding)
    {}

    struct TagPath {};
    TypeRef(TagPath, Span sp, AST::Path path):
        m_span(mv$(sp)),
        m_data(TypeData::make_Path({ ::std::move(path) }))
    {}
    TypeRef(Span sp, AST::Path path):
        TypeRef(TagPath(), mv$(sp), mv$(path))
    {}

    TypeRef( Span sp, ::std::vector<Type_TraitPath> traits, ::std::vector<AST::LifetimeRef> lifetimes ):
        m_span(mv$(sp)),
        m_data(TypeData::make_TraitObject({ ::std::move(traits), mv$(lifetimes) }))
    {}


    const Span& span() const { return m_span; }

    bool is_valid() const { return ! m_data.is_None(); }

    bool is_unbounded() const { return m_data.is_Any(); }
    bool is_wildcard() const { return m_data.is_Any(); }

    bool is_unit() const { return m_data.is_Unit(); }
    bool is_primitive() const { return m_data.is_Primitive(); }

    bool is_path() const { return m_data.is_Path(); }
    const AST::Path& path() const { return m_data.as_Path().path; }
    AST::Path& path() { return m_data.as_Path().path; }

    bool is_type_param() const { return m_data.is_Generic(); }
    const ::std::string& type_param() const { return m_data.as_Generic().name; }

    bool is_reference() const { return m_data.is_Borrow(); }
    bool is_pointer() const { return m_data.is_Pointer(); }
    bool is_tuple() const { return m_data.is_Tuple(); }

    TypeRef clone() const;

    const TypeRef& inner_type() const {
        TU_MATCH_DEF(TypeData, (m_data), (e),
        ( throw ::std::runtime_error("Called inner_type on non-wrapper"); ),
        (Borrow,  return *e.inner; ),
        (Pointer, return *e.inner; ),
        (Array,   return *e.inner; )
        )
    }
    TypeRef& inner_type() {
        TU_MATCH_DEF(TypeData, (m_data), (e),
        ( throw ::std::runtime_error("Called inner_type on non-wrapper"); ),
        (Borrow,  return *e.inner; ),
        (Pointer, return *e.inner; ),
        (Array,   return *e.inner; )
        )
    }

    Ordering ord(const TypeRef& x) const;
    bool operator==(const TypeRef& x) const { return ord(x) == OrdEqual; }
    bool operator!=(const TypeRef& x) const { return ord(x) != OrdEqual; }
    bool operator<(const TypeRef& x) const { return ord(x) == OrdLess; };

    void print(::std::ostream& os, bool is_debug=false) const;

    PrettyPrintType print_pretty() const { return PrettyPrintType(*this); }

    friend class PrettyPrintType;

    friend ::std::ostream& operator<<(::std::ostream& os, const TypeRef& tr);
};

#endif // TYPES_HPP_INCLUDED
