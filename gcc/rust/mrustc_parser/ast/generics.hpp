/*
 * MRustC - Rust Compiler
 * - By John Hodge (Mutabah/thePowersGang)
 *
 * ast/generics.hpp
 * - AST Generics (type parameters, bounds, ...)
 */
#pragma once

#include <string>
#include "types.hpp"

namespace AST {

class TypeParam
{
    ::AST::AttributeList    m_attrs;
    Span    m_span;
    // TODO: use an Ident?
    ::std::string   m_name;
    ::TypeRef m_default;
public:
    TypeParam(TypeParam&& x) = default;
    TypeParam& operator=(TypeParam&& x) = default;
    explicit TypeParam(const TypeParam& x):
        m_attrs( x.m_attrs ),
        m_span( x.m_span ),
        m_name( x.m_name ),
        m_default( x.m_default.clone() )
    {
    }

    TypeParam(Span sp, ::AST::AttributeList attrs, ::std::string name):
        m_attrs( ::std::move(attrs) ),
        m_span( ::std::move(sp) ),
        m_name( ::std::move(name) ),
        m_default(m_span)
    {}

    void setDefault(TypeRef type) {
        assert(m_default.is_wildcard());
        m_default = ::std::move(type);
    }

    const ::AST::AttributeList& attrs() const { return m_attrs; }
    const Span& span() const { return m_span; }
    const ::std::string& name() const { return m_name; }

    const TypeRef& get_default() const { return m_default; }
          TypeRef& get_default()       { return m_default; }

    friend ::std::ostream& operator<<(::std::ostream& os, const TypeParam& tp);
};
class LifetimeParam
{
    ::AST::AttributeList    m_attrs;
    Span    m_span;
    Ident   m_name;
public:
    LifetimeParam(Span sp, ::AST::AttributeList attrs, Ident name):
        m_attrs( ::std::move(attrs) ),
        m_span( ::std::move(sp) ),
        m_name( ::std::move(name) )
    {
    }
    LifetimeParam(LifetimeParam&&) = default;
    LifetimeParam& operator=(LifetimeParam&&) = default;
    explicit LifetimeParam(const LifetimeParam&) = default;

    const ::AST::AttributeList& attrs() const { return m_attrs; }
    const Span& span() const { return m_span; }
    const Ident& name() const { return m_name; }

    friend ::std::ostream& operator<<(::std::ostream& os, const LifetimeParam& p);
};

// HigherRankedBounds is defined in `types.hpp`

TAGGED_UNION_EX( GenericBound, (), None,
    (
    (None, struct{}),
    // Lifetime bound: 'test must be valid for 'bound
    (Lifetime, struct {
        LifetimeRef test;
        LifetimeRef bound;
        }),
    // Type lifetime bound
    (TypeLifetime, struct {
        TypeRef type;
        LifetimeRef bound;
        }),
    // Standard trait bound: "Type: [for<'a>] Trait"
    (IsTrait, struct {
        HigherRankedBounds  outer_hrbs;
        TypeRef type;
        HigherRankedBounds  inner_hrbs;
        AST::Path   trait;
        }),
    // Removed trait bound: "Type: ?Trait"
    (MaybeTrait, struct {
        TypeRef type;
        AST::Path   trait;
        }),
    // Negative trait bound: "Type: !Trait"
    (NotTrait, struct {
        TypeRef type;
        AST::Path   trait;
        }),
    // Type equality: "Type = Replacement"
    (Equality, struct {
        TypeRef type;
        TypeRef replacement;
        })
    ),

    (, span(x.span) ), ( span = x.span; ),
    (
    public:

        Span    span;

        GenericBound clone() const {
            TU_MATCH(GenericBound, ( (*this) ), (ent),
            (None, return make_None({}); ),
            (Lifetime,     return make_Lifetime({ent.test, ent.bound});     ),
            (TypeLifetime, return make_TypeLifetime({ent.type.clone(), ent.bound}); ),
            (IsTrait,    return make_IsTrait({ent.outer_hrbs, ent.type.clone(), ent.inner_hrbs, ent.trait}); ),
            (MaybeTrait, return make_MaybeTrait({ent.type.clone(), ent.trait}); ),
            (NotTrait,   return make_NotTrait({ent.type.clone(), ent.trait}); ),
            (Equality,   return make_Equality({ent.type.clone(), ent.replacement.clone()}); )
            )
            return GenericBound();
        }
        )
    );

::std::ostream& operator<<(::std::ostream& os, const GenericBound& x);

class GenericParams
{
    ::std::vector<TypeParam>    m_type_params;
    ::std::vector<LifetimeParam>    m_lifetime_params;
    ::std::vector<GenericBound>    m_bounds;
public:
    GenericParams() {}
    GenericParams(GenericParams&& x) = default;
    GenericParams& operator=(GenericParams&& x) = default;
    GenericParams(const GenericParams& x) = delete;

    GenericParams clone() const {
        GenericParams   rv;
        rv.m_type_params = ::std::vector<TypeParam>( m_type_params );   // Copy-constructable
        rv.m_lifetime_params = ::std::vector<LifetimeParam>(m_lifetime_params);
        rv.m_bounds.reserve( m_bounds.size() );
        for(auto& e: m_bounds)
            rv.m_bounds.push_back( e.clone() );
        return rv;
    }

    const ::std::vector<TypeParam>& ty_params() const { return m_type_params; }
          ::std::vector<TypeParam>& ty_params()       { return m_type_params; }
    const ::std::vector<LifetimeParam>& lft_params() const { return m_lifetime_params; }
    const ::std::vector<GenericBound>& bounds() const { return m_bounds; }
          ::std::vector<GenericBound>& bounds()       { return m_bounds; }

    void add_ty_param(TypeParam param) { m_type_params.push_back( ::std::move(param) ); }
    void add_lft_param(LifetimeParam lft) { m_lifetime_params.push_back( ::std::move(lft) ); }
    void add_bound(GenericBound bound) {
        m_bounds.push_back( ::std::move(bound) );
    }

    int find_name(const char* name) const;
    bool check_params(Crate& crate, const ::std::vector<TypeRef>& types) const;
    bool check_params(Crate& crate, ::std::vector<TypeRef>& types, bool allow_infer) const;

    friend ::std::ostream& operator<<(::std::ostream& os, const GenericParams& tp);
};


}

