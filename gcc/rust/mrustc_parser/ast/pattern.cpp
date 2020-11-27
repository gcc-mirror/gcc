/*
 * MRustC - Rust Compiler
 * - By John Hodge (Mutabah/thePowersGang)
 *
 * ast/pattern.cpp
 * - AST::Pattern support/implementation code
 */
#include "../common.hpp"
#include "ast.hpp"
#include "pattern.hpp"

namespace AST {

::std::ostream& operator<<(::std::ostream& os, const Pattern::Value& val)
{
    TU_MATCH(Pattern::Value, (val), (e),
    (Invalid,
        os << "/*BAD PAT VAL*/";
        ),
    (Integer,
        switch(e.type)
        {
        case CORETYPE_BOOL:
            os << (e.value ? "true" : "false");
            break;
        case CORETYPE_F32:
        case CORETYPE_F64:
            BUG(Span(), "Hit F32/f64 in printing pattern literal");
            break;
        default:
            os << e.value;
            break;
        }
        ),
    (Float,
        switch(e.type)
        {
        case CORETYPE_BOOL:
            os << (e.value ? "true" : "false");
            break;
        case CORETYPE_ANY:
        case CORETYPE_F32:
        case CORETYPE_F64:
            os << e.value;
            break;
        default:
            BUG(Span(), "Hit integer in printing pattern literal");
            break;
        }
        ),
    (String,
        os << "\"" << e << "\"";
        ),
    (ByteString,
        os << "b\"" << e.v << "\"";
        ),
    (Named,
        os << e;
        )
    )
    return os;
}
::std::ostream& operator<<(::std::ostream& os, const Pattern::TuplePat& val)
{
    if( val.has_wildcard )
    {
        os << val.start;
        os << ".., ";
        os << val.end;
    }
    else
    {
        os << val.start;
        assert(val.end.size() == 0);
    }
    return os;
}
::std::ostream& operator<<(::std::ostream& os, const PatternBinding& pb)
{
    if( pb.m_mutable )
        os << "mut ";
    switch(pb.m_type)
    {
    case PatternBinding::Type::MOVE:    break;
    case PatternBinding::Type::REF:     os << "ref ";   break;
    case PatternBinding::Type::MUTREF:  os << "ref mut ";   break;
    }
    os << pb.m_name;
    return os;
}
::std::ostream& operator<<(::std::ostream& os, const Pattern& pat)
{
    if( pat.m_binding.is_valid() ) {
        os << pat.m_binding << " @ ";
    }
    TU_MATCH(Pattern::Data, (pat.m_data), (ent),
    (MaybeBind,
        os << ent.name << "?";
        ),
    (Macro,
        os << *ent.inv;
        ),
    (Any,
        os << "_";
        ),
    (Box,
        os << "box " << *ent.sub;
        ),
    (Ref,
        os << "&" << (ent.mut ? "mut " : "") << *ent.sub;
        ),
    (Value,
        os << ent.start;
        if( ! ent.end.is_Invalid() )
            os << " ... " << ent.end;
        ),
    (Tuple,
        os << "(" << ent << ")";
        ),
    (StructTuple,
        os << ent.path << " (" << ent.tup_pat << ")";
        ),
    (Struct,
        os << ent.path << " {" << ent.sub_patterns << "}";
        ),
    (Slice,
        os << "[";
        os << ent.sub_pats;
        os << "]";
        ),
    (SplitSlice,
        os << "[";
        bool needs_comma = false;
        if(ent.leading.size()) {
            os << ent.leading;
            needs_comma = true;
        }

        if( needs_comma ) {
            os << ", ";
        }
        if( ent.extra_bind.is_valid() )
            os << ent.extra_bind;
        os << "..";
        needs_comma = true;

        if(ent.trailing.size()) {
            if( needs_comma ) {
                os << ", ";
            }
            os << ent.trailing;
        }
        os << "]";
        )
    )
    return os;
}

Pattern::~Pattern()
{
}

AST::Pattern AST::Pattern::clone() const
{
    AST::Pattern    rv;
    rv.m_span = m_span;
    rv.m_binding = PatternBinding(m_binding);

    struct H {
        static ::std::unique_ptr<Pattern> clone_sp(const ::std::unique_ptr<Pattern>& p) {
            return ::std::make_unique<Pattern>( p->clone() );
        }
        static ::std::vector<Pattern> clone_list(const ::std::vector<Pattern>& list) {
            ::std::vector<Pattern>  rv;
            rv.reserve(list.size());
            for(const auto& p : list)
                rv.push_back( p.clone() );
            return rv;
        }
        static TuplePat clone_tup(const TuplePat& p) {
            return TuplePat {
                H::clone_list(p.start),
                p.has_wildcard,
                H::clone_list(p.end)
                };
        }
        static AST::Pattern::Value clone_val(const AST::Pattern::Value& v) {
            TU_MATCH(::AST::Pattern::Value, (v), (e),
            (Invalid, return Value(e);),
            (Integer, return Value(e);),
            (Float, return Value(e);),
            (String, return Value(e);),
            (ByteString, return Value(e);),
            (Named, return Value::make_Named( AST::Path(e) );)
            )
            throw "";
        }
    };

    TU_MATCH(Pattern::Data, (m_data), (e),
    (Any,
        rv.m_data = Data::make_Any(e);
        ),
    (MaybeBind,
        rv.m_data = Data::make_MaybeBind(e);
        ),
    (Macro,
        rv.m_data = Data::make_Macro({ ::std::make_unique<AST::MacroInvocation>( e.inv->clone() ) });
        ),
    (Box,
        rv.m_data = Data::make_Box({ H::clone_sp(e.sub) });
        ),
    (Ref,
        rv.m_data = Data::make_Ref({ e.mut, H::clone_sp(e.sub) });
        ),
    (Value,
        rv.m_data = Data::make_Value({ H::clone_val(e.start), H::clone_val(e.end) });
        ),
    (Tuple,
        rv.m_data = Data::make_Tuple( H::clone_tup(e) );
        ),
    (StructTuple,
        rv.m_data = Data::make_StructTuple({ ::AST::Path(e.path), H::clone_tup(e.tup_pat) });
        ),
    (Struct,
        ::std::vector< ::std::pair< ::std::string, Pattern> >   sps;
        for(const auto& sp : e.sub_patterns)
            sps.push_back( ::std::make_pair(sp.first, sp.second.clone()) );
        rv.m_data = Data::make_Struct({ ::AST::Path(e.path), mv$(sps) });
        ),
    (Slice,
        rv.m_data = Data::make_Slice({ H::clone_list(e.sub_pats) });
        ),
    (SplitSlice,
        rv.m_data = Data::make_SplitSlice({ H::clone_list(e.leading), e.extra_bind, H::clone_list(e.trailing) });
        )
    )

    return rv;
}

}   // namespace AST

