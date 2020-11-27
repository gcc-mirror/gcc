/*
 * MRustC - Mutabah's Rust Compiler
 * - By John Hodge (Mutabah/thePowersGang)
 *
 * ast/expr.cpp
 * - AST Expression nodes
 */
#include "expr.hpp"
#include "ast.hpp"

namespace AST {


Expr::Expr(unique_ptr<ExprNode> node):
    m_node(node.release())
{
}
Expr::Expr(ExprNode* node):
    m_node(node)
{
}
Expr::Expr():
    m_node(nullptr)
{
}
void Expr::visit_nodes(NodeVisitor& v)
{
    if( m_node )
    {
        m_node->visit(v);
    }
}
void Expr::visit_nodes(NodeVisitor& v) const
{
    if( m_node )
    {
        assert(v.is_const());
        //const_cast<const ExprNode*>(m_node.get())->visit(v);
        m_node->visit(v);
    }
}

Expr Expr::clone() const
{
    if( m_node ) {
        return Expr( m_node->clone() );
    }
    else {
        return Expr();
    }
}

::std::ostream& operator<<(::std::ostream& os, const Expr& pat)
{
    if( pat.m_node.get() )
        return os << *pat.m_node;
    else
        return os << "/* null */";
}

::std::ostream& operator<<(::std::ostream& os, const ExprNode& node)
{
    assert( static_cast<const void*>(&node) != nullptr );
    node.print(os);
    return os;
}
ExprNode::~ExprNode() {
}

#define NODE(class, _print, _clone)\
    void class::visit(NodeVisitor& nv) { nv.visit(*this); } \
    void class::print(::std::ostream& os) const _print \
    ::std::unique_ptr<ExprNode> class::clone() const _clone
#define OPT_CLONE(node) (node.get() ? node->clone() : ::AST::ExprNodeP())

namespace {
    static inline ExprNodeP mk_exprnodep(const Span& pos, AST::ExprNode* en) {
        en->set_span(pos);
        return ExprNodeP(en);
    }
    #define NEWNODE(type, ...)  mk_exprnodep(span(), new type(__VA_ARGS__))
}

NODE(ExprNode_Block, {
    os << "{";
    for(const auto& n : m_nodes)
        os << *n << ";";
    os << "}";
},{
    ::std::vector<ExprNodeP>    nodes;
    for(const auto& n : m_nodes)
        nodes.push_back( n->clone() );
    return NEWNODE(ExprNode_Block, m_is_unsafe, m_yields_final_value, mv$(nodes), m_local_mod);
})

NODE(ExprNode_Macro, {
    os << m_name << "!";
    if( m_ident.size() > 0 )
    {
        os << " " << m_ident << " ";
    }
    os << "(" << " /*TODO*/ " << ")";
},{
    return NEWNODE(ExprNode_Macro, m_name, m_ident, m_tokens.clone());
})

NODE(ExprNode_Asm, {
    os << "asm!( \"" << m_text << "\"";
    os << " :";
    for(const auto& v : m_output)
        os << " \"" << v.name << "\" (" << *v.value << "),";
    os << " :";
    for(const auto& v : m_input)
        os << " \"" << v.name << "\" (" << *v.value << "),";
    os << " :";
    for(const auto& v : m_clobbers)
        os << " \"" << v << "\",";
    os << " :";
    for(const auto& v : m_flags)
        os << " \"" << v << "\",";
    os << " )";
},{
    ::std::vector<ExprNode_Asm::ValRef> outputs;
    for(const auto& v : m_output)
        outputs.push_back( ExprNode_Asm::ValRef { v.name, v.value->clone() });
    ::std::vector<ExprNode_Asm::ValRef> inputs;
    for(const auto& v : m_input)
        inputs.push_back( ExprNode_Asm::ValRef { v.name, v.value->clone() });
    return NEWNODE(ExprNode_Asm, m_text, mv$(outputs), mv$(inputs), m_clobbers, m_flags);
})

NODE(ExprNode_Flow, {
    switch(m_type)
    {
    case RETURN:    os << "return"; break;
    case BREAK:     os << "break"; break;
    case CONTINUE:  os << "continue"; break;
    }
    if(m_value)
        os << " " << *m_value;
},{
    return NEWNODE(ExprNode_Flow, m_type, m_target, m_value ? m_value->clone() : nullptr);
})


NODE(ExprNode_LetBinding, {
    os << "let " << m_pat << ": " << m_type;
    if(m_value)
        os << " = " << *m_value;
},{
    return NEWNODE(ExprNode_LetBinding, m_pat.clone(), m_type.clone(), OPT_CLONE(m_value));
})

NODE(ExprNode_Assign, {
    os << *m_slot << " = " << *m_value;
},{
    return NEWNODE(ExprNode_Assign, m_op, m_slot->clone(), m_value->clone());
})

NODE(ExprNode_CallPath, {
    os << m_path << "(";
    for(const auto& a : m_args) {
        os << *a << ",";
    }
    os << ")";
},{
    ::std::vector<ExprNodeP>    args;
    for(const auto& a : m_args) {
        args.push_back( a->clone() );
    }
    return NEWNODE(ExprNode_CallPath, AST::Path(m_path), mv$(args));
})

NODE(ExprNode_CallMethod, {
    os << "(" << *m_val << ")." << m_method << "(";
    for(const auto& a : m_args) {
        os << *a << ",";
    }
    os << ")";
},{
    ::std::vector<ExprNodeP>    args;
    for(const auto& a : m_args) {
        args.push_back( a->clone() );
    }
    return NEWNODE(ExprNode_CallMethod, m_val->clone(), m_method, mv$(args));
})

NODE(ExprNode_CallObject, {
    os << "(" << *m_val << ")(";
    for(const auto& a : m_args) {
        os << *a << ",";
    }
    os << ")";
},{
    ::std::vector<ExprNodeP>    args;
    for(const auto& a : m_args) {
        args.push_back( a->clone() );
    }
    return NEWNODE(ExprNode_CallObject, m_val->clone(), mv$(args));
})

NODE(ExprNode_Loop, {
    os << "LOOP [" << m_label << "] " << m_pattern;
    if(m_cond)
        os << " in/= " << *m_cond;
    os << " " << *m_code;
},{
    return NEWNODE(ExprNode_Loop, m_label, m_type, m_pattern.clone(), OPT_CLONE(m_cond), m_code->clone());
})

NODE(ExprNode_Match, {
    os << "match ("<<*m_val<<") {";
    for(const auto& arm : m_arms)
    {
        for( const auto& pat : arm.m_patterns )
            os << " " << pat;
        if( arm.m_cond )
            os << " if " << *arm.m_cond;

        os << " => " << *arm.m_code << ",";
    }
    os << "}";
},{
    ::std::vector< ExprNode_Match_Arm>  arms;
    for(const auto& arm : m_arms) {
        ::std::vector< AST::Pattern>    patterns;
        for( const auto& pat : arm.m_patterns ) {
            patterns.push_back( pat.clone() );
        }
        arms.push_back( ExprNode_Match_Arm( mv$(patterns), OPT_CLONE(arm.m_cond), arm.m_code->clone() ) );
        arms.back().m_attrs = arm.m_attrs.clone();
    }
    return NEWNODE(ExprNode_Match, m_val->clone(), mv$(arms));
})

NODE(ExprNode_If, {
    os << "if " << *m_cond << " { " << *m_true << " }";
    if(m_false)
        os << " else { " << *m_false << " }";
},{
    return NEWNODE(ExprNode_If, m_cond->clone(), m_true->clone(), OPT_CLONE(m_false));
})
NODE(ExprNode_IfLet, {
    os << "if let " << m_pattern << " = (" << *m_value << ") { " << *m_true << " }";
    if(m_false) os << " else { " << *m_false << " }";
},{
    return NEWNODE(ExprNode_IfLet, m_pattern.clone(), m_value->clone(), m_true->clone(), OPT_CLONE(m_false));
})

NODE(ExprNode_Integer, {
    if( m_datatype == CORETYPE_CHAR )
        os << "'\\u{" << ::std::hex << m_value << ::std::dec << "}'";
    else
    {
        os << m_value;
        if( m_datatype == CORETYPE_ANY )
            ;
        else
            os << "_" << coretype_name(m_datatype);
    }
},{
    return NEWNODE(ExprNode_Integer, m_value, m_datatype);
})
NODE(ExprNode_Float, {
    os << m_value << "_" << m_datatype;
},{
    return NEWNODE(ExprNode_Float, m_value, m_datatype);
})
NODE(ExprNode_Bool, {
    os << m_value;
},{
    return NEWNODE(ExprNode_Bool, m_value);
})
NODE(ExprNode_String, {
    os << "\"" << m_value << "\"";
},{
    return NEWNODE(ExprNode_String, m_value);
})
NODE(ExprNode_ByteString, {
    os << "b\"" << m_value << "\"";
},{
    return NEWNODE(ExprNode_ByteString, m_value);
})

NODE(ExprNode_Closure, {
    if( m_is_move )
        os << "move ";
    os << "|";
    for(const auto& a : m_args)
    {
        os << a.first << ": " << a.second << ",";
    }
    os << "|";
    os << "->" << m_return;
    os << " " << *m_code;
},{
    ExprNode_Closure::args_t    args;
    for(const auto& a : m_args) {
        args.push_back( ::std::make_pair(a.first.clone(), a.second.clone()) );
    }
    return NEWNODE(ExprNode_Closure, mv$(args), m_return.clone(), m_code->clone(), m_is_move);
});

NODE(ExprNode_StructLiteral, {
    os << m_path << " { ";
    for(const auto& v : m_values)
    {
        os << v.name << ": " << *v.value << ", ";
    }
    if(m_base_value)
    {
        os << ".." << *m_base_value;
    }
    os << "}";
},{
    ExprNode_StructLiteral::t_values    vals;

    for(const auto& v : m_values) {
        vals.push_back({ v.attrs.clone(), v.name, v.value->clone() });
    }

    return NEWNODE(ExprNode_StructLiteral, AST::Path(m_path), OPT_CLONE(m_base_value), mv$(vals) );
})

NODE(ExprNode_Array, {
    os << "[";
    if( m_size.get() )
        os << *m_values[0] << "; " << *m_size;
    else
        for(const auto& a : m_values)
            os << *a << ",";
    os << "]";
},{
    if( m_size.get() )
    {
        return NEWNODE(ExprNode_Array, m_values[0]->clone(), m_size->clone());
    }
    else
    {
        ::std::vector<ExprNodeP>    nodes;
        for(const auto& n : m_values)
            nodes.push_back( n->clone() );
        return NEWNODE(ExprNode_Array, mv$(nodes));
    }
})

NODE(ExprNode_Tuple, {
    os << "(";
    for(const auto& a : m_values) {
        os << *a << ",";
    }
    os << ")";
},{
    ::std::vector<ExprNodeP>    nodes;
    for(const auto& n : m_values)
        nodes.push_back( n->clone() );
    return NEWNODE(ExprNode_Tuple, mv$(nodes));
})

NODE(ExprNode_NamedValue, {
    os << m_path;
},{
    return NEWNODE(ExprNode_NamedValue, AST::Path(m_path));
})

NODE(ExprNode_Field, {
    os << "(" << *m_obj << ")." << m_name;
},{
    return NEWNODE(ExprNode_Field, m_obj->clone(), m_name);
})

NODE(ExprNode_Index, {
    os << "(" << *m_obj << ")[" << *m_idx << "]";
},{
    return NEWNODE(ExprNode_Index, m_obj->clone(), m_idx->clone());
})

NODE(ExprNode_Deref, {
    os << "*(" << *m_value << ")";
},{
    return NEWNODE(ExprNode_Deref, m_value->clone());
});

NODE(ExprNode_Cast, {
    os << "(" << *m_value << " as " << m_type << ")";
},{
    return NEWNODE(ExprNode_Cast, m_value->clone(), m_type.clone());
})
NODE(ExprNode_TypeAnnotation, {
    os << "(" << *m_value << ": " << m_type << ")";
},{
    return NEWNODE(ExprNode_TypeAnnotation, m_value->clone(), m_type.clone());
})

NODE(ExprNode_BinOp, {
    if( m_type == RANGE_INC ) {
        os << "(";
        if( m_left ) {
            os << *m_left << " ";
        }
        os << "... " << *m_right;
        os << ")";
        return ;
    }
    if( m_type == RANGE ) {
        os << "(";
        if( m_left ) {
            os << *m_left;
        }
        os << "..";
        if( m_right ) {
            os << " " << *m_right;
        }
        os << ")";
        return ;
    }
    os << "(" << *m_left << " ";
    switch(m_type)
    {
    case CMPEQU:    os << "=="; break;
    case CMPNEQU:   os << "!="; break;
    case CMPLT:     os << "<";  break;
    case CMPLTE:    os << "<="; break;
    case CMPGT:     os << ">";  break;
    case CMPGTE:    os << ">="; break;
    case BOOLAND:   os << "&&"; break;
    case BOOLOR:    os << "||"; break;
    case BITAND:    os << "&"; break;
    case BITOR:     os << "|"; break;
    case BITXOR:    os << "^"; break;
    case SHR:    os << ">>"; break;
    case SHL:    os << "<<"; break;
    case MULTIPLY: os << "*"; break;
    case DIVIDE:   os << "/"; break;
    case MODULO:   os << "%"; break;
    case ADD:   os << "+"; break;
    case SUB:   os << "-"; break;
    case RANGE:   os << ".."; break;
    case RANGE_INC:   os << "..."; break;
    case PLACE_IN:  os << "<-"; break;
    }
    os << " " << *m_right << ")";
},{
    return NEWNODE(ExprNode_BinOp, m_type, OPT_CLONE(m_left), OPT_CLONE(m_right));
})

NODE(ExprNode_UniOp, {
    switch(m_type)
    {
    case NEGATE: os << "(-"; break;
    case INVERT: os << "(!"; break;
    case BOX: os << "(box "; break;
    case REF: os << "(&"; break;
    case REFMUT: os << "(&mut "; break;
    case QMARK: os << "(" << *m_value << "?)"; return;
    }
    os << *m_value << ")";
},{
    return NEWNODE(ExprNode_UniOp, m_type, m_value->clone());
})


#define NV(type, actions)\
    void NodeVisitorDef::visit(type& node) { /*DEBUG("DEF - "#type);*/ actions }
//  void NodeVisitorDef::visit(const type& node) { DEBUG("DEF - "#type" (const)"); actions }

NV(ExprNode_Block, {
    //INDENT();
    for( auto& child : node.m_nodes )
        visit(child);
    //UNINDENT();
})
NV(ExprNode_Macro,
{
    BUG(node.span(), "Hit unexpanded macro in expression - " << node);
})
NV(ExprNode_Asm,
{
    for(auto& v : node.m_output)
        visit(v.value);
    for(auto& v : node.m_input)
        visit(v.value);
})
NV(ExprNode_Flow,
{
    visit(node.m_value);
})
NV(ExprNode_LetBinding,
{
    // TODO: Handle recurse into Let pattern?
    visit(node.m_value);
})
NV(ExprNode_Assign,
{
    INDENT();
    visit(node.m_slot);
    visit(node.m_value);
    UNINDENT();
})
NV(ExprNode_CallPath,
{
    INDENT();
    for( auto& arg : node.m_args )
        visit(arg);
    UNINDENT();
})
NV(ExprNode_CallMethod,
{
    INDENT();
    visit(node.m_val);
    for( auto& arg : node.m_args )
        visit(arg);
    UNINDENT();
})
NV(ExprNode_CallObject,
{
    INDENT();
    visit(node.m_val);
    for( auto& arg : node.m_args )
        visit(arg);
    UNINDENT();
})
NV(ExprNode_Loop,
{
    INDENT();
    visit(node.m_cond);
    visit(node.m_code);
    UNINDENT();
})
NV(ExprNode_Match,
{
    INDENT();
    visit(node.m_val);
    for( auto& arm : node.m_arms )
    {
        visit(arm.m_cond);
        visit(arm.m_code);
    }
    UNINDENT();
})
NV(ExprNode_If,
{
    INDENT();
    visit(node.m_cond);
    visit(node.m_true);
    visit(node.m_false);
    UNINDENT();
})
NV(ExprNode_IfLet,
{
    INDENT();
    visit(node.m_value);
    visit(node.m_true);
    visit(node.m_false);
    UNINDENT();
})

NV(ExprNode_Integer, {(void)node;})
NV(ExprNode_Float, {(void)node;})
NV(ExprNode_Bool, {(void)node;})
NV(ExprNode_String, {(void)node;})
NV(ExprNode_ByteString, {(void)node;})

NV(ExprNode_Closure,
{
    visit(node.m_code);
});
NV(ExprNode_StructLiteral,
{
    visit(node.m_base_value);
    for( auto& val : node.m_values )
        visit(val.value);
})
NV(ExprNode_Array,
{
    visit(node.m_size);
    for( auto& val : node.m_values )
        visit(val);
})
NV(ExprNode_Tuple,
{
    for( auto& val : node.m_values )
        visit(val);
})
NV(ExprNode_NamedValue,
{
    (void)node;
    // LEAF
})

NV(ExprNode_Field,
{
    visit(node.m_obj);
})
NV(ExprNode_Index,
{
    visit(node.m_obj);
    visit(node.m_idx);
})
NV(ExprNode_Deref,
{
    visit(node.m_value);
})
NV(ExprNode_Cast,
{
    visit(node.m_value);
})
NV(ExprNode_TypeAnnotation,
{
    visit(node.m_value);
})
NV(ExprNode_BinOp,
{
    visit(node.m_left);
    visit(node.m_right);
})
NV(ExprNode_UniOp,
{
    visit(node.m_value);
})
#undef NV


};

