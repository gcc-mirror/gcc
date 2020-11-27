/*
 * MRustC - Rust Compiler
 * - By John Hodge (Mutabah/thePowersGang)
 *
 * ast/ast.cpp
 * - Implementation of the various AST classes
 */
#include "ast.hpp"
#include "crate.hpp"
#include "types.hpp"
#include "expr.hpp"
#include "../common.hpp"
#include <iostream>
#include "../parse/parseerror.hpp"
#include <algorithm>

namespace AST {


namespace {
    ::std::vector<Attribute> clone_mivec(const ::std::vector<Attribute>& v) {
        ::std::vector<Attribute>    ri;
        ri.reserve(v.size());
        for(const auto& i : v)
            ri.push_back( i.clone() );
        return ri;
    }
}

AttributeList AttributeList::clone() const
{
    return AttributeList( clone_mivec(m_items) );
}

void AttributeList::push_back(Attribute i)
{
    m_items.push_back( ::std::move(i) );
}
const Attribute* AttributeList::get(const char *name) const
{
    for( auto& i : m_items ) {
        if(i.name() == name) {
            //i.mark_used();
            return &i;
        }
    }
    return 0;
}

Attribute Attribute::clone() const
{
    TU_MATCHA( (m_data), (e),
    (None,
        return Attribute(m_span, m_name);
        ),
    (String,
        return Attribute(m_span, m_name, e.val);
        ),
    (List,
        return Attribute(m_span, m_name, clone_mivec(e.sub_items));
        )
    )
    throw ::std::runtime_error("Attribute::clone - Fell off end");
}

StructItem StructItem::clone() const
{
    return StructItem(m_attrs.clone(), m_is_public, m_name, m_type.clone());
}
TupleItem TupleItem::clone() const
{
    return TupleItem(m_attrs.clone(), m_is_public, m_type.clone());
}


TypeAlias TypeAlias::clone() const
{
    return TypeAlias( m_params.clone(), m_type.clone() );
}
Static Static::clone() const
{
    return Static( m_class, m_type.clone(), m_value.is_valid() ? AST::Expr( m_value.node().clone() ) : AST::Expr() );
}

Function::Function(Span sp, GenericParams params, ::std::string abi, bool is_unsafe, bool is_const, bool is_variadic, TypeRef ret_type, Arglist args):
    m_span(sp),
    m_params( move(params) ),
    m_rettype( move(ret_type) ),
    m_args( move(args) ),
    m_abi( mv$(abi) ),
    m_is_const(is_const),
    m_is_unsafe(is_unsafe),
    m_is_variadic(is_variadic)
{
}
Function Function::clone() const
{
    decltype(m_args)    new_args;
    for(const auto& arg : m_args)
        new_args.push_back( ::std::make_pair( arg.first.clone(), arg.second.clone() ) );

    auto rv = Function( m_span, m_params.clone(), m_abi, m_is_unsafe, m_is_const, m_is_variadic, m_rettype.clone(), mv$(new_args) );
    if( m_code.is_valid() )
    {
        rv.m_code = AST::Expr( m_code.node().clone() );
    }
    return rv;
}

void Trait::add_type(::std::string name, AttributeList attrs, TypeRef type) {
    m_items.push_back( Named<Item>(mv$(name), Item::make_Type({TypeAlias(GenericParams(), mv$(type))}), true) );
    m_items.back().data.attrs = mv$(attrs);
}
void Trait::add_function(::std::string name, AttributeList attrs, Function fcn) {
    DEBUG("trait fn " << name);
    m_items.push_back( Named<Item>(mv$(name), Item::make_Function({mv$(fcn)}), true) );
    m_items.back().data.attrs = mv$(attrs);
}
void Trait::add_static(::std::string name, AttributeList attrs, Static v) {
    m_items.push_back( Named<Item>(mv$(name), Item::make_Static({mv$(v)}), true) );
    m_items.back().data.attrs = mv$(attrs);
}
void Trait::set_is_marker() {
    m_is_marker = true;
}
bool Trait::is_marker() const {
    return m_is_marker;
}
bool Trait::has_named_item(const ::std::string& name, bool& out_is_fcn) const
{
    for( const auto& i : m_items )
    {
        if( i.name == name ) {
            out_is_fcn = i.data.is_Function();
            return true;
        }
    }
    return false;
}

Trait Trait::clone() const
{
    auto rv = Trait(m_params.clone(), m_supertraits);
    for(const auto& item : m_items)
    {
        rv.m_items.push_back( Named<Item> { item.name, item.data.clone(), item.is_pub } );
    }
    return rv;
}

Enum Enum::clone() const
{
    decltype(m_variants)    new_variants;
    for(const auto& var : m_variants)
    {
        TU_MATCHA( (var.m_data), (e),
        (Value,
            new_variants.push_back( EnumVariant(var.m_attrs.clone(), var.m_name, e.m_value.clone()) );
            ),
        (Tuple,
            decltype(e.m_sub_types) new_st;
            for(const auto& f : e.m_sub_types)
                new_st.push_back( f.clone() );
            new_variants.push_back( EnumVariant(var.m_attrs.clone(), var.m_name, mv$(new_st)) );
            ),
        (Struct,
            decltype(e.m_fields)    new_fields;
            for(const auto& f : e.m_fields)
                new_fields.push_back( f.clone() );
            new_variants.push_back( EnumVariant(var.m_attrs.clone(), var.m_name, mv$(new_fields)) );
            )
        )
    }
    return Enum(m_params.clone(), mv$(new_variants));
}
Struct Struct::clone() const
{
    TU_MATCHA( (m_data), (e),
    (Unit,
        return Struct(m_params.clone());
        ),
    (Tuple,
        decltype(e.ents)    new_fields;
        for(const auto& f : e.ents)
            new_fields.push_back( f.clone() );
        return Struct(m_params.clone(), mv$(new_fields));
        ),
    (Struct,
        decltype(e.ents)    new_fields;
        for(const auto& f : e.ents)
            new_fields.push_back( f.clone() );
        return Struct(m_params.clone(), mv$(new_fields));
        )
    )
    throw "";
}

Union Union::clone() const
{
    decltype(m_variants)    new_vars;
    for(const auto& f : m_variants)
        new_vars.push_back( f.clone() );
    return Union(m_params.clone(), mv$(new_vars));
}

::std::ostream& operator<<(::std::ostream& os, const ImplDef& impl)
{
    return os << "impl<" << impl.m_params << "> " << impl.m_trait.ent << " for " << impl.m_type << "";
}

void Impl::add_function(bool is_public, bool is_specialisable, ::std::string name, Function fcn)
{
    DEBUG("impl fn " << name);
    m_items.push_back( ImplItem { is_public, is_specialisable, mv$(name), box$( Item::make_Function(mv$(fcn)) ) } );
}
void Impl::add_type(bool is_public, bool is_specialisable, ::std::string name, TypeRef type)
{
    m_items.push_back( ImplItem { is_public, is_specialisable, mv$(name), box$( Item::make_Type(TypeAlias(GenericParams(), mv$(type))) ) } );
}
void Impl::add_static(bool is_public, bool is_specialisable, ::std::string name, Static v)
{
    m_items.push_back( ImplItem { is_public, is_specialisable, mv$(name), box$( Item::make_Static(mv$(v)) ) } );
}
void Impl::add_macro_invocation(MacroInvocation item) {
    m_items.push_back( ImplItem { false, false, "", box$( Item::make_MacroInv(mv$(item)) ) } );
}

bool Impl::has_named_item(const ::std::string& name) const
{
    for( const auto& it : this->items() )
    {
        if( it.name == name ) {
            return true;
        }
    }
    return false;
}

::std::ostream& operator<<(::std::ostream& os, const Impl& impl)
{
    return os << impl.m_def;
}

::std::ostream& operator<<(::std::ostream& os, const UseStmt& x)
{
    os << "Use(" << x.path << ")";
    return os;
}



MacroInvocation MacroInvocation::clone() const
{
    return MacroInvocation(m_span, m_macro_name, m_ident, m_input.clone());
}


UseStmt UseStmt::clone() const
{
    return UseStmt(sp, path);
}

void ExternBlock::add_item(Named<Item> named_item)
{
    ASSERT_BUG(named_item.data.span, named_item.data.is_Function() || named_item.data.is_Static(), "Incorrect item type for ExternBlock");
    m_items.push_back( mv$(named_item) );
}
ExternBlock ExternBlock::clone() const
{
    TODO(Span(), "Clone an extern block");
}

::std::shared_ptr<AST::Module> Module::add_anon() {
    auto rv = ::std::shared_ptr<AST::Module>( new Module(m_my_path + FMT("#" << m_anon_modules.size())) );
    DEBUG("New anon " << rv->m_my_path);
    rv->m_file_info = m_file_info;

    m_anon_modules.push_back( rv );

    return rv;
}

void Module::add_item( Named<Item> named_item ) {
    m_items.push_back( mv$(named_item) );
    const auto& i = m_items.back();
    if( i.name == "" ) {
    }
    else {
        DEBUG(m_my_path << "::" << i.name << " = " << i.data.tag_str() << ", attrs = " << i.data.attrs);
    }
}
void Module::add_item(bool is_pub, ::std::string name, Item it, AttributeList attrs) {
    it.attrs = mv$(attrs);
    add_item( Named<Item>( mv$(name), mv$(it), is_pub ) );
}
void Module::add_ext_crate(bool is_public, ::std::string ext_name, ::std::string imp_name, AttributeList attrs) {
    this->add_item( is_public, imp_name, Item::make_Crate({mv$(ext_name)}), mv$(attrs) );
}
void Module::add_alias(bool is_public, UseStmt us, ::std::string name, AttributeList attrs) {
    this->add_item( is_public, mv$(name), Item(mv$(us)), mv$(attrs) );
}
void Module::add_macro_invocation(MacroInvocation item) {
    this->add_item( false, "", Item( mv$(item) ), ::AST::AttributeList {} );
}
void Module::add_macro(bool is_exported, ::std::string name, MacroRulesPtr macro) {
    m_macros.push_back( Named<MacroRulesPtr>( mv$(name), mv$(macro), is_exported ) );
}
void Module::add_macro_import(::std::string name, const MacroRules& mr) {
    m_macro_import_res.push_back( Named<const MacroRules*>( mv$(name), &mr, false ) );
}

Item Item::clone() const
{
    TU_MATCHA( (*this), (e),
    (None,
        return AST::Item(e);
        ),
    (MacroInv,
        TODO(this->span, "Clone on Item::MacroInv");
        ),
    (Use,
        return AST::Item(e.clone());
        ),
    (ExternBlock,
        TODO(this->span, "Clone on Item::" << this->tag_str());
        ),
    (Impl,
        TODO(this->span, "Clone on Item::Impl");
        ),
    (NegImpl,
        TODO(this->span, "Clone on Item::NegImpl");
        ),
    (Module,
        TODO(this->span, "Clone on Item::Module");
        ),
    (Crate,
        return AST::Item(e);
        ),
    (Type,
        return AST::Item(e.clone());
        ),
    (Struct,
        return AST::Item(e.clone());
        ),
    (Enum,
        return AST::Item(e.clone());
        ),
    (Union,
        return AST::Item(e.clone());
        ),
    (Trait,
        return AST::Item(e.clone());
        ),

    (Function,
        return AST::Item(e.clone());
        ),
    (Static,
        return AST::Item(e.clone());
        )
    )
    throw "";
}



::std::ostream& operator<<(::std::ostream& os, const TypeParam& tp)
{
    //os << "TypeParam(";
    os << tp.m_name;
    os << " = ";
    os << tp.m_default;
    //os << ")";
    return os;
}
::std::ostream& operator<<(::std::ostream& os, const LifetimeParam& p)
{
    os << "'" << p.m_name;
    return os;
}

::std::ostream& operator<<(::std::ostream& os, const HigherRankedBounds& x)
{
    if( x.m_lifetimes.empty() ) {
        return os;
    }
    os << "for<";
    for(const auto& l : x.m_lifetimes)
        os << "'" << l << ",";
    os << "> ";
    return os;
}


::std::ostream& operator<<(::std::ostream& os, const GenericBound& x)
{
    TU_MATCH(GenericBound, (x), (ent),
    (None,
        os << "/*-*/";
        ),
    (Lifetime,
        os << "'" << ent.test << ": '" << ent.bound;
        ),
    (TypeLifetime,
        os << ent.type << ": '" << ent.bound;
        ),
    (IsTrait,
        os << ent.outer_hrbs << ent.type << ": " << ent.inner_hrbs << ent.trait;
        ),
    (MaybeTrait,
        os << ent.type << ": ?" << ent.trait;
        ),
    (NotTrait,
        os << ent.type << ": !" << ent.trait;
        ),
    (Equality,
        os << ent.type << " = " << ent.replacement;
        )
    )
    return os;
}


int GenericParams::find_name(const char* name) const
{
    for( unsigned int i = 0; i < m_type_params.size(); i ++ )
    {
        if( m_type_params[i].name() == name )
            return i;
    }
    DEBUG("Type param '" << name << "' not in list");
    return -1;
}

::std::ostream& operator<<(::std::ostream& os, const GenericParams& tps)
{
    return os << "<" << tps.m_lifetime_params << "," << tps.m_type_params << "> where {" << tps.m_bounds << "}";
}

}    // namespace AST
