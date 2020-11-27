/*
 * MRustC - Rust Compiler
 * - By John Hodge (Mutabah/thePowersGang)
 *
 * ast/attrs.hpp
 * - AST Attributes (#[foo] and #![foo])
 */
#ifndef _AST_ATTRS_HPP_
#define _AST_ATTRS_HPP_


namespace AST {

//
class Attribute;
::std::ostream& operator<<(::std::ostream& os, const Attribute& x);

/// A list of attributes on an item (searchable by the attribute name)
class AttributeList
{
public:
    ::std::vector<Attribute> m_items;

    AttributeList() {}
    AttributeList(::std::vector<Attribute> items):
        m_items( mv$(items) )
    {
    }

    // Move present
    AttributeList(AttributeList&&) = default;
    AttributeList& operator=(AttributeList&&) = default;
    // No copy assign, but explicit copy
    explicit AttributeList(const AttributeList&) = default;
    AttributeList& operator=(const AttributeList&) = delete;
    // Explicit clone
    AttributeList clone() const;

    void push_back(Attribute i);

    const Attribute* get(const char *name) const;
    Attribute* get(const char *name) {
        return const_cast<Attribute*>( const_cast<const AttributeList*>(this)->get(name));
    }
    bool has(const char *name) const {
        return get(name) != 0;
    }

    friend ::std::ostream& operator<<(::std::ostream& os, const AttributeList& x) {
        for(const auto& i : x.m_items) {
            os << "#[" << i << "]";
        }
        return os;
    }
};


TAGGED_UNION(AttributeData, None,
    (None, struct {}),
    (String, struct {
        ::std::string   val;
        }),
    (List, struct {
        ::std::vector<Attribute> sub_items;
        })
    );

// An attribute can has a name, and optional data:
// Data can be:
// - A parenthesised token tree
//   > In 1.19 this was actually just sub-attributes
// - an associated (string) literal

class Attribute
{
    Span    m_span;
    ::std::string   m_name;
    AttributeData   m_data;
    mutable bool    m_is_used;
public:
    Attribute(Span sp, ::std::string name):
        m_span(::std::move(sp)),
        m_name(name),
        m_data( AttributeData::make_None({}) )
    {
    }
    Attribute(Span sp, ::std::string name, ::std::string str_val):
        m_span(::std::move(sp)),
        m_name(name),
        m_data( AttributeData::make_String({mv$(str_val)}) )
    {
    }
    Attribute(Span sp, ::std::string name, ::std::vector<Attribute> items):
        m_span(::std::move(sp)),
        m_name(name),
        m_data( AttributeData::make_List({mv$(items)}) )
    {
    }

    explicit Attribute(const Attribute& x):
        m_span(x.m_span),
        m_name(x.m_name),
        m_is_used(x.m_is_used)
    {
        TU_MATCHA( (x.m_data), (e),
        (None,
            ),
        (String,
            m_data = AttributeData::make_String({ e.val });
            ),
        (List,
            m_data = AttributeData::make_List({ ::std::vector<Attribute>(e.sub_items) });
            )
        )
    }
    Attribute& operator=(const Attribute& ) = delete;
    Attribute(Attribute&& ) = default;
    Attribute& operator=(Attribute&& ) = default;
    Attribute clone() const;

    void mark_used() const { m_is_used = true; }
    bool is_used() const { return m_is_used; }

    const Span& span() const { return m_span; }
    const ::std::string& name() const { return m_name; }
    const AttributeData& data() const { return m_data; }

    // Legacy accessors/checkers
    bool has_noarg() const { return m_data.is_None(); }

    bool has_string() const { return m_data.is_String(); }
    const ::std::string& string() const { return m_data.as_String().val; }

    bool has_sub_items() const { return m_data.is_List(); }
    const ::std::vector<Attribute>& items() const { return m_data.as_List().sub_items; }
          ::std::vector<Attribute>& items()       { return m_data.as_List().sub_items; }

    friend ::std::ostream& operator<<(::std::ostream& os, const Attribute& x) {
        os << x.m_name;
        TU_MATCHA( (x.m_data), (e),
        (None,
            ),
        (String,
            os << "=\"" << e.val << "\"";
            ),
        (List,
            os << "(" << e.sub_items << ")";
            )
        )
        return os;
    }
};

}   // namespace AST

#endif

