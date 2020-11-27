/*
 * MRustC - Mutabah's Rust Compiler
 * - By John Hodge (Mutabah/thePowersGang)
 *
 * ast/path.hpp
 * - AST::Path and helper types
 */
#ifndef AST_PATH_HPP_INCLUDED
#define AST_PATH_HPP_INCLUDED

#include "../common.hpp"
#include <string>
#include <stdexcept>
#include <vector>
#include <initializer_list>
#include <cassert>
#include <tagged_union.hpp>
#include <string>
#include "../include/span.hpp"
#include "../include/ident.hpp"

class TypeRef;

namespace HIR {
class Module;
class Trait;
class Enum;
class Struct;
class Union;
class Static;
}   // namespace HIR

namespace AST {

class LifetimeRef;
class GenericParams;
class Crate;
class Module;
class TypeAlias;
class Enum;
class Struct;
class Union;
class Trait;
class Static;
class Function;
class ExternCrate;

TAGGED_UNION_EX(PathBinding, (), Unbound, (
    (Unbound, struct {
        }),
    (Crate, struct {
        const ExternCrate* crate_;
        }),
    (Module, struct {
        const Module* module_;
        const ::HIR::Module* hir;
        }),
    (Struct, struct {
        const Struct* struct_;
        const ::HIR::Struct* hir;
        }),
    (Enum,   struct {
        const Enum* enum_;
        const ::HIR::Enum*  hir;
        }),
    (Union,   struct {
        const Union* union_;
        const ::HIR::Union*  hir;
        }),
    (Trait,  struct {
        const Trait* trait_;
        const ::HIR::Trait* hir;
        }),
    (Static, struct {
        const Static* static_;
        const ::HIR::Static* hir; // if nullptr and static_ == nullptr, points to a `const`
        }),
    (Function, struct {
        const Function* func_;
        }),
    (EnumVar, struct {
        const Enum* enum_;
        unsigned int idx;
        const ::HIR::Enum*  hir;
        }),
    (TypeAlias, struct {
        const TypeAlias* alias_;
        }),
    (StructMethod, struct {
        const Struct* struct_;
        ::std::string name;
        }),
    (TraitMethod, struct {
        const Trait* trait_;
        ::std::string name;
        }),

    (TypeParameter, struct {
        unsigned int level;
        unsigned int idx;
        }),
    (Variable, struct {
        unsigned int slot;
        })
    ),
    (), (),
    (
    public:
        PathBinding clone() const;
        )
    );

extern ::std::ostream& operator<<(::std::ostream& os, const PathBinding& x);

struct PathParams
{
    ::std::vector< LifetimeRef >  m_lifetimes;
    ::std::vector< TypeRef >    m_types;
    ::std::vector< ::std::pair< ::std::string, TypeRef> >   m_assoc;

    PathParams(PathParams&& x) = default;
    PathParams(const PathParams& x);
    PathParams() {}
    PathParams(::std::vector<LifetimeRef> lfts, ::std::vector<TypeRef> tys, ::std::vector<::std::pair<::std::string,TypeRef>> a):
        m_lifetimes(mv$(lfts)),
        m_types(mv$(tys)),
        m_assoc(mv$(a))
    {}

    PathParams& operator=(PathParams&& x) = default;
    PathParams& operator=(const PathParams& x) = delete;

    bool is_empty() const {
        return m_lifetimes.empty() && m_types.empty() && m_assoc.empty();
    }

    Ordering ord(const PathParams& x) const;

    friend ::std::ostream& operator<<(::std::ostream& os, const PathParams& x);
};

class PathNode
{
    ::std::string   m_name;
    PathParams  m_params;
public:
    PathNode() {}
    PathNode(::std::string name, PathParams args = {});
    const ::std::string& name() const { return m_name; }

    const ::AST::PathParams& args() const { return m_params; }
          ::AST::PathParams& args()       { return m_params; }

    Ordering ord(const PathNode& x) const;
    void print_pretty(::std::ostream& os, bool is_type_context) const;

    bool operator==(const PathNode& x) const { return ord(x) == OrdEqual; }
    friend ::std::ostream& operator<<(::std::ostream& os, const PathNode& pn);
};

class Path
{
public:
    TAGGED_UNION(Class, Invalid,
        (Invalid, struct {}),
        (Local, struct {   // Variable / Type param (resolved)
            ::std::string name;
            } ),
        (Relative, struct {    // General relative
            Ident::Hygiene hygiene;
            ::std::vector<PathNode> nodes;
            } ),
        (Self, struct {    // Module-relative
            ::std::vector<PathNode> nodes;
            } ),
        (Super, struct {   // Parent-relative
            unsigned int count; // Number of `super` keywords, must be >= 1
            ::std::vector<PathNode> nodes;
            } ),
        (Absolute, struct {    // Absolute
            ::std::string   crate;
            ::std::vector<PathNode> nodes;
            } ),
        (UFCS, struct {    // Type-relative
            ::std::unique_ptr<TypeRef> type;    // always non-null
            ::std::unique_ptr<Path> trait;   // nullptr = inherent, Invalid = unknown trait
            ::std::vector<PathNode> nodes;
            } )
        );

public:
    Class   m_class;

private:
    PathBinding m_binding;
public:
    virtual ~Path();
    // INVALID
    Path():
        m_class()
    {}
    Path(Path&&) = default;
    Path& operator=(AST::Path&& x) {
        m_class = mv$(x.m_class);
        m_binding = mv$(x.m_binding);
        //DEBUG("Path, " << x);
        return *this;
    }

    Path(const Path& x);
    Path& operator=(const AST::Path&) = delete;

    // ABSOLUTE
    Path(::std::string crate, ::std::vector<PathNode> nodes):
        m_class( Class::make_Absolute({ mv$(crate), mv$(nodes)}) )
    {}

    // UFCS
    struct TagUfcs {};
    Path(TagUfcs, TypeRef type, ::std::vector<PathNode> nodes={});
    Path(TagUfcs, TypeRef type, Path trait, ::std::vector<PathNode> nodes={});

    // VARIABLE
    struct TagLocal {};
    Path(TagLocal, ::std::string name):
        m_class( Class::make_Local({ mv$(name) }) )
    {}
    Path(::std::string name):
        m_class( Class::make_Local({ mv$(name) }) )
    {}

    // RELATIVE
    struct TagRelative {};
    Path(TagRelative, Ident::Hygiene hygiene, ::std::vector<PathNode> nodes):
        m_class( Class::make_Relative({ mv$(hygiene), mv$(nodes) }) )
    {}
    // SELF
    struct TagSelf {};
    Path(TagSelf, ::std::vector<PathNode> nodes):
        m_class( Class::make_Self({ mv$(nodes) }) )
    {}
    // SUPER
    struct TagSuper {};
    Path(TagSuper, unsigned int count, ::std::vector<PathNode> nodes):
        m_class( Class::make_Super({ count, mv$(nodes) }) )
    {}

    //void set_crate(::std::string crate) {
    //    if( m_crate == "" ) {
    //        m_crate = crate;
    //        DEBUG("crate set to " << m_crate);
    //    }
    //}


    Class::Tag class_tag() const {
        return m_class.tag();
    }

    Path operator+(PathNode pn) const {
        Path tmp = Path(*this);
        tmp.nodes().push_back( mv$(pn) );
        return tmp;
    }
    Path operator+(const ::std::string& s) const {
        Path tmp = Path(*this);
        tmp.append(PathNode(s, {}));
        return tmp;
    }
    Path operator+(const Path& x) const {
        return Path(*this) += x;
    }
    Path& operator+=(const Path& x);

    void append(PathNode node) {
        assert( !m_class.is_Invalid() );
        //if( m_class.is_Invalid() )
        //    m_class = Class::make_Relative({});
        nodes().push_back( mv$(node) );
        m_binding = PathBinding();
    }

    bool is_trivial() const {
        TU_MATCH_DEF(Class, (m_class), (e),
        (
            return false;
            ),
        (Local,
            return true;
            ),
        (Relative,
            return e.nodes.size() == 1 && e.nodes[0].args().is_empty();
            )
        )
    }

    bool is_valid() const { return !m_class.is_Invalid(); }
    bool is_absolute() const { return m_class.is_Absolute(); }
    bool is_relative() const { return m_class.is_Relative() || m_class.is_Super() || m_class.is_Self(); }

    size_t size() const {
        TU_MATCH(Class, (m_class), (ent),
        (Invalid,  assert(!m_class.is_Invalid()); throw ::std::runtime_error("Path::nodes() on Invalid"); ),
        (Local,    return 1;),
        (Relative, return ent.nodes.size();),
        (Self,     return ent.nodes.size();),
        (Super,    return ent.nodes.size();),
        (Absolute, return ent.nodes.size();),
        (UFCS,     return ent.nodes.size();)
        )
        throw ::std::runtime_error("Path::nodes() fell off");
    }
    //const ::std::string& crate() const { return m_crate; }

    bool is_concrete() const;

    bool is_bound() const { return !m_binding.is_Unbound(); }
    const PathBinding& binding() const { return m_binding; }
    void bind_variable(unsigned int slot);

    ::std::vector<PathNode>& nodes() {
        TU_MATCH(Class, (m_class), (ent),
        (Invalid,  assert(!m_class.is_Invalid()); throw ::std::runtime_error("Path::nodes() on Invalid"); ),
        (Local,    assert(!m_class.is_Local()); throw ::std::runtime_error("Path::nodes() on Local"); ),
        (Relative, return ent.nodes;),
        (Self,     return ent.nodes;),
        (Super,    return ent.nodes;),
        (Absolute, return ent.nodes;),
        (UFCS,     return ent.nodes;)
        )
        throw ::std::runtime_error("Path::nodes() fell off");
    }
    const ::std::vector<PathNode>& nodes() const {
        return ((Path*)this)->nodes();
    }

    PathNode& operator[](int idx) { if(idx>=0) return nodes()[idx]; else return nodes()[size()+idx]; }
    const PathNode& operator[](int idx) const { return (*(Path*)this)[idx]; }

    Ordering ord(const Path& x) const;
    bool operator==(const Path& x) const { return ord(x) == OrdEqual; }
    bool operator!=(const Path& x) const { return ord(x) != OrdEqual; }
    bool operator<(const Path& x) const { return ord(x) != OrdLess; }

    void print_pretty(::std::ostream& os, bool is_type_context, bool is_debug=false) const;
    friend ::std::ostream& operator<<(::std::ostream& os, const Path& path);
private:
    static void resolve_args_nl(::std::vector<PathNode>& nodes, ::std::function<TypeRef(const char*)> fcn);

    void check_param_counts(const GenericParams& params, bool expect_params, PathNode& node);
public:
    void bind_enum_var(const Enum& ent, const ::std::string& name, const ::std::vector<TypeRef>& args={});
    void bind_function(const Function& ent, const ::std::vector<TypeRef>& args={}) {
        (void)args;
        m_binding = PathBinding::make_Function({&ent});
    }

    void bind(::AST::PathBinding pb) {
        m_binding = mv$(pb);
    }
};

}   // namespace AST

#endif
