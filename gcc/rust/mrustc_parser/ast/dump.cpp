/*
 * MRustC - Mutabah's Rust Compiler
 * - By John Hodge (Mutabah/thePowersGang)
 *
 * ast/dump.cpp
 * - Dumps the AST of a crate as rust code (annotated)
 */
#include <ast/crate.hpp>
#include <ast/ast.hpp>
#include <ast/expr.hpp>
#include <main_bindings.hpp>
#include <hir/hir.hpp>  // ABI_RUST - TODO: Move elsewhere?
#include <fstream>

#include <cpp_unpack.h>

#define IS(v, c)    (dynamic_cast<c*>(&v) != 0)
#define WRAPIF_CMD(v, t)  || IS(v, t)
#define WRAPIF(uniq_ptr, class1, ...) do { auto& _v = *(uniq_ptr); if( IS(_v, class1) CC_ITERATE(WRAPIF_CMD, (_v), __VA_ARGS__) ) { paren_wrap(uniq_ptr); } else { AST::NodeVisitor::visit(uniq_ptr); } } while(0)

class RustPrinter:
    public AST::NodeVisitor
{
    ::std::ostream& m_os;
    int m_indent_level;
    bool m_expr_root;   //!< used to allow 'if' and 'match' to behave differently as standalone exprs
public:
    RustPrinter(::std::ostream& os):
        m_os(os),
        m_indent_level(0),
        m_expr_root(false)
    {}

    void handle_module(const AST::Module& mod);
    void handle_struct(const AST::Struct& s);
    void handle_enum(const AST::Enum& s);
    void handle_trait(const AST::Trait& s);

    void handle_function(bool is_pub, const ::std::string& name, const AST::Function& f);

    virtual bool is_const() const override { return true; }
    virtual void visit(AST::ExprNode_Block& n) override {
        if( n.m_is_unsafe ) {
            m_os << "unsafe ";
        }
        m_os << "{";
        inc_indent();
        if( n.m_local_mod )
        {
            handle_module(*n.m_local_mod);
        }
        bool is_first = true;
        for( auto& child : n.m_nodes )
        {
            if(is_first) {
                is_first = false;
            } else {
                m_os << ";";
            }
            m_os << "\n";
            m_os << indent();
            m_expr_root = true;
            if( !child.get() )
                m_os << "/* nil */";
            else
                AST::NodeVisitor::visit(child);
        }
        if( !n.m_yields_final_value )
            m_os << ";";
        m_os << "\n";
        dec_indent();
        m_os << indent() << "}";
    }
    virtual void visit(AST::ExprNode_Macro& n) override {
        m_expr_root = false;
        m_os << n.m_name << "!( /* TODO: Macro TT */ )";
    }
    virtual void visit(AST::ExprNode_Asm& n) override {
        m_os << "asm!( \"" << n.m_text << "\"";
        m_os << " :";
        for(const auto& v : n.m_output)
        {
            m_os << " \"" << v.name << "\" (";
            AST::NodeVisitor::visit(v.value);
            m_os << "),";
        }
        m_os << " :";
        for(const auto& v : n.m_input)
        {
            m_os << " \"" << v.name << "\" (";
            AST::NodeVisitor::visit(v.value);
            m_os << "),";
        }
        m_os << " :";
        for(const auto& v : n.m_clobbers)
            m_os << " \"" << v << "\",";
        m_os << " :";
        for(const auto& v : n.m_flags)
            m_os << " \"" << v << "\",";
        m_os << " )";
    }
    virtual void visit(AST::ExprNode_Flow& n) override {
        m_expr_root = false;
        switch(n.m_type)
        {
        case AST::ExprNode_Flow::RETURN:    m_os << "return ";  break;
        case AST::ExprNode_Flow::BREAK:     m_os << "break ";  break;
        case AST::ExprNode_Flow::CONTINUE:  m_os << "continue ";  break;
        }
        AST::NodeVisitor::visit(n.m_value);
    }
    virtual void visit(AST::ExprNode_LetBinding& n) override {
        m_expr_root = false;
        m_os << "let ";
        print_pattern(n.m_pat, false);
        m_os << ": ";
        print_type(n.m_type);
        m_os << " = ";
        AST::NodeVisitor::visit(n.m_value);
    }
    virtual void visit(AST::ExprNode_Assign& n) override {
        m_expr_root = false;
        AST::NodeVisitor::visit(n.m_slot);
        switch(n.m_op)
        {
        case AST::ExprNode_Assign::NONE:    m_os << "  = ";  break;
        case AST::ExprNode_Assign::ADD:     m_os << " += ";  break;
        case AST::ExprNode_Assign::SUB:     m_os << " -= ";  break;
        case AST::ExprNode_Assign::MUL:     m_os << " *= ";  break;
        case AST::ExprNode_Assign::DIV:     m_os << " /= ";  break;
        case AST::ExprNode_Assign::MOD:     m_os << " %= ";  break;
        case AST::ExprNode_Assign::AND:     m_os << " &= ";  break;
        case AST::ExprNode_Assign::OR:      m_os << " |= ";  break;
        case AST::ExprNode_Assign::XOR:     m_os << " ^= ";  break;
        case AST::ExprNode_Assign::SHR:     m_os << " >>= ";  break;
        case AST::ExprNode_Assign::SHL:     m_os << " <<= ";  break;
        }
        AST::NodeVisitor::visit(n.m_value);
    }
    virtual void visit(AST::ExprNode_CallPath& n) override {
        m_expr_root = false;
        m_os << n.m_path;
        m_os << "(";
        bool is_first = true;
        for( auto& arg : n.m_args )
        {
            if(is_first) {
                is_first = false;
            } else {
                m_os << ", ";
            }
            AST::NodeVisitor::visit(arg);
        }
        m_os << ")";
    }
    virtual void visit(AST::ExprNode_CallMethod& n) override {
        m_expr_root = false;
        WRAPIF( n.m_val
            , AST::ExprNode_Deref, AST::ExprNode_UniOp
            , AST::ExprNode_Cast, AST::ExprNode_BinOp, AST::ExprNode_Assign
            , AST::ExprNode_Match, AST::ExprNode_If, AST::ExprNode_IfLet, AST::ExprNode_Match
            );
        m_os << "." << n.m_method;
        m_os << "(";
        bool is_first = true;
        for( auto& arg : n.m_args )
        {
            if(is_first) {
                is_first = false;
            } else {
                m_os << ", ";
            }
            AST::NodeVisitor::visit(arg);
        }
        m_os << ")";
    }
    virtual void visit(AST::ExprNode_CallObject& n) override {
        m_expr_root = false;
        m_os << "(";
        AST::NodeVisitor::visit(n.m_val);
        m_os << ")(";
        bool is_first = true;
        for( auto& arg : n.m_args )
        {
            if(is_first) {
                is_first = false;
            } else {
                m_os << ", ";
            }
            AST::NodeVisitor::visit(arg);
        }
        m_os << ")";
    }
    virtual void visit(AST::ExprNode_Loop& n) override {
        bool expr_root = m_expr_root;
        m_expr_root = false;

        switch(n.m_type)
        {
        case AST::ExprNode_Loop::LOOP:
            m_os << "loop";
            break;
        case AST::ExprNode_Loop::WHILE:
            m_os << "while ";
            AST::NodeVisitor::visit(n.m_cond);
            break;
        case AST::ExprNode_Loop::WHILELET:
            m_os << "while let ";
            print_pattern(n.m_pattern, true);
            m_os << " = ";
            AST::NodeVisitor::visit(n.m_cond);
            break;
        case AST::ExprNode_Loop::FOR:
            m_os << "while for ";
            print_pattern(n.m_pattern, true);
            m_os << " in ";
            AST::NodeVisitor::visit(n.m_cond);
            break;
        }

        if( expr_root )
        {
            m_os << "\n";
            m_os << indent();
        }
        else
        {
            m_os << " ";
        }

        AST::NodeVisitor::visit(n.m_code);
    }
    virtual void visit(AST::ExprNode_Match& n) override {
        bool expr_root = m_expr_root;
        m_expr_root = false;
        m_os << "match ";
        AST::NodeVisitor::visit(n.m_val);

        if(expr_root)
        {
            m_os << "\n";
            m_os << indent() << "{\n";
        }
        else
        {
            m_os << " {\n";
            inc_indent();
        }

        for( auto& arm : n.m_arms )
        {
            m_os << indent();
            bool is_first = true;
            for( const auto& pat : arm.m_patterns ) {
                if(!is_first)
                    m_os << "|";
                is_first = false;
                print_pattern(pat, true);
            }
            if( arm.m_cond )
            {
                m_os << " if ";
                AST::NodeVisitor::visit(arm.m_cond);
            }
            m_os << " => ";
            // Increase indent, but don't print. Causes nested blocks to be indented above the match
            inc_indent();
            AST::NodeVisitor::visit(arm.m_code);
            dec_indent();
            m_os << ",\n";
        }

        if(expr_root)
        {
            m_os << indent() << "}";
        }
        else
        {
            m_os << indent() << "}";
            dec_indent();
        }
    }
    virtual void visit(AST::ExprNode_If& n) override {
        bool expr_root = m_expr_root;
        m_expr_root = false;
        m_os << "if ";
        AST::NodeVisitor::visit(n.m_cond);

        visit_if_common(expr_root, n.m_true, n.m_false);
    }
    virtual void visit(AST::ExprNode_IfLet& n) override {
        bool expr_root = m_expr_root;
        m_expr_root = false;
        m_os << "if let ";
        print_pattern(n.m_pattern, true);
        m_os << " = ";
        AST::NodeVisitor::visit(n.m_value);

        visit_if_common(expr_root, n.m_true, n.m_false);
    }
    void visit_if_common(bool expr_root, const ::std::unique_ptr<AST::ExprNode>& tv, const ::std::unique_ptr<AST::ExprNode>& fv)
    {
        if( expr_root )
        {
            m_os << "\n";
            m_os << indent();
        }
        else
        {
            m_os << " ";
        }

        bool is_block = (dynamic_cast<const AST::ExprNode_Block*>(&*tv) != nullptr);
        if( !is_block ) m_os << "{ ";
        AST::NodeVisitor::visit(tv);
        if( !is_block ) m_os << " }";
        if(fv.get())
        {
            if( expr_root )
            {
                m_os << "\n";
                m_os << indent() << "else";
                // handle chained if statements nicely
                if( IS(*fv, AST::ExprNode_If) || IS(*fv, AST::ExprNode_IfLet) ) {
                    m_expr_root = true;
                    m_os << " ";
                }
                else
                    m_os << "\n" << indent();
            }
            else
            {
                m_os << " else ";
            }
            AST::NodeVisitor::visit(fv);
        }
    }
    virtual void visit(AST::ExprNode_Closure& n) override {
        m_expr_root = false;
        m_os << "|";
        bool is_first = true;
        for( const auto& arg : n.m_args )
        {
            if(!is_first)   m_os << ", ";
            is_first = false;
            print_pattern(arg.first, false);
            m_os << ": ";
            print_type(arg.second);
        }
        m_os << "| ->";
        print_type(n.m_return);
        m_os << " ";
        AST::NodeVisitor::visit(n.m_code);
    }
    virtual void visit(AST::ExprNode_Integer& n) override {
        m_expr_root = false;
        switch(n.m_datatype)
        {
        case CORETYPE_INVAL:
            m_os << "0x" << ::std::hex << n.m_value << ::std::dec << "_/*INVAL*/";
            break;
        case CORETYPE_BOOL:
        case CORETYPE_STR:
            m_os << "0x" << ::std::hex << n.m_value << ::std::dec << "_/*bool/str*/";
            break;
        case CORETYPE_CHAR:
            m_os << "'\\u{" << ::std::hex << n.m_value << ::std::dec << "}'";
            break;
        case CORETYPE_F32:
        case CORETYPE_F64:
            break;
        case CORETYPE_U8:
        case CORETYPE_U16:
        case CORETYPE_U32:
        case CORETYPE_U64:
        case CORETYPE_U128:
        case CORETYPE_UINT:
        case CORETYPE_ANY:
            m_os << "0x" << ::std::hex << n.m_value << ::std::dec;
            break;
        case CORETYPE_I8:
        case CORETYPE_I16:
        case CORETYPE_I32:
        case CORETYPE_I64:
        case CORETYPE_I128:
        case CORETYPE_INT:
            m_os << (int64_t)n.m_value;
            break;
        }
    }
    virtual void visit(AST::ExprNode_Float& n) override {
        m_expr_root = false;
        switch(n.m_datatype)
        {
        case CORETYPE_ANY:
        case CORETYPE_F32:
        case CORETYPE_F64:
            m_os.precision(10);
            m_os << n.m_value;
            break;
        default:
            break;
        }
    }
    virtual void visit(AST::ExprNode_Bool& n) override {
        m_expr_root = false;
        if( n.m_value )
            m_os << "true";
        else
            m_os << "false";
    }
    virtual void visit(AST::ExprNode_String& n) override {
        m_expr_root = false;
        m_os << "\"" << n.m_value << "\"";
    }
    virtual void visit(AST::ExprNode_ByteString& n) override {
        m_expr_root = false;
        m_os << "b\"" << n.m_value << "\"";
    }

    virtual void visit(AST::ExprNode_StructLiteral& n) override {
        m_expr_root = false;
        m_os << n.m_path << " {\n";
        inc_indent();
        for( const auto& i : n.m_values )
        {
            // TODO: Attributes
            m_os << indent() << i.name << ": ";
            AST::NodeVisitor::visit(i.value);
            m_os << ",\n";
        }
        if( n.m_base_value.get() )
        {
            m_os << indent() << ".. ";
            AST::NodeVisitor::visit(n.m_base_value);
            m_os << "\n";
        }
        m_os << indent() << "}";
        dec_indent();
    }
    virtual void visit(AST::ExprNode_Array& n) override {
        m_expr_root = false;
        m_os << "[";
        if( n.m_size.get() )
        {
            AST::NodeVisitor::visit(n.m_values[0]);
            m_os << "; ";
            AST::NodeVisitor::visit(n.m_size);
        }
        else {
            for( auto& item : n.m_values )
            {
                AST::NodeVisitor::visit(item);
                m_os << ", ";
            }
        }
        m_os << "]";
    }
    virtual void visit(AST::ExprNode_Tuple& n) override {
        m_expr_root = false;
        m_os << "(";
        for( auto& item : n.m_values )
        {
            AST::NodeVisitor::visit(item);
            m_os << ", ";
        }
        m_os << ")";
    }
    virtual void visit(AST::ExprNode_NamedValue& n) override {
        m_expr_root = false;
        m_os << n.m_path;
    }
    virtual void visit(AST::ExprNode_Field& n) override {
        m_expr_root = false;
        WRAPIF( n.m_obj
            , AST::ExprNode_Deref, AST::ExprNode_UniOp
            , AST::ExprNode_Cast, AST::ExprNode_BinOp, AST::ExprNode_Assign
            , AST::ExprNode_Match, AST::ExprNode_If, AST::ExprNode_IfLet, AST::ExprNode_Match
            );
        m_os << "." << n.m_name;
    }
    virtual void visit(AST::ExprNode_Index& n) override {
        m_expr_root = false;
        WRAPIF( n.m_obj
            , AST::ExprNode_Deref, AST::ExprNode_UniOp
            , AST::ExprNode_Cast, AST::ExprNode_BinOp, AST::ExprNode_Assign
            , AST::ExprNode_Match, AST::ExprNode_If, AST::ExprNode_IfLet, AST::ExprNode_Match
            );
        m_os << "[";
        AST::NodeVisitor::visit(n.m_idx);
        m_os << "]";
    }
    virtual void visit(AST::ExprNode_Deref& n) override {
        m_expr_root = false;
        m_os << "*(";
        AST::NodeVisitor::visit(n.m_value);
        m_os << ")";
    }
    virtual void visit(AST::ExprNode_Cast& n) override {
        m_expr_root = false;
        AST::NodeVisitor::visit(n.m_value);
        m_os << " as " << n.m_type;
    }
    virtual void visit(AST::ExprNode_TypeAnnotation& n) override {
        m_expr_root = false;
        AST::NodeVisitor::visit(n.m_value);
        m_os << ": " << n.m_type;
    }
    virtual void visit(AST::ExprNode_BinOp& n) override {
        m_expr_root = false;
        if( IS(*n.m_left, AST::ExprNode_BinOp) && dynamic_cast<AST::ExprNode_BinOp&>(*n.m_left).m_type == n.m_type ) {
            AST::NodeVisitor::visit(n.m_left);
        }
        else {
            WRAPIF(n.m_left
                , AST::ExprNode_Cast, AST::ExprNode_BinOp
                );
        }
        m_os << " ";
        switch(n.m_type)
        {
        case AST::ExprNode_BinOp::CMPEQU: m_os << "=="; break;
        case AST::ExprNode_BinOp::CMPNEQU:m_os << "!="; break;
        case AST::ExprNode_BinOp::CMPLT:  m_os << "<";  break;
        case AST::ExprNode_BinOp::CMPLTE: m_os << "<="; break;
        case AST::ExprNode_BinOp::CMPGT:  m_os << ">";  break;
        case AST::ExprNode_BinOp::CMPGTE: m_os << ">="; break;
        case AST::ExprNode_BinOp::BOOLAND:m_os << "&&"; break;
        case AST::ExprNode_BinOp::BOOLOR: m_os << "||"; break;
        case AST::ExprNode_BinOp::BITAND: m_os << "&";  break;
        case AST::ExprNode_BinOp::BITOR:  m_os << "|";  break;
        case AST::ExprNode_BinOp::BITXOR: m_os << "^";  break;
        case AST::ExprNode_BinOp::SHL:    m_os << "<<"; break;
        case AST::ExprNode_BinOp::SHR:    m_os << ">>"; break;
        case AST::ExprNode_BinOp::MULTIPLY: m_os << "*"; break;
        case AST::ExprNode_BinOp::DIVIDE:   m_os << "/"; break;
        case AST::ExprNode_BinOp::MODULO:   m_os << "%"; break;
        case AST::ExprNode_BinOp::ADD:   m_os << "+"; break;
        case AST::ExprNode_BinOp::SUB:   m_os << "-"; break;
        case AST::ExprNode_BinOp::RANGE: m_os << ".."; break;
        case AST::ExprNode_BinOp::RANGE_INC: m_os << "..."; break;
        case AST::ExprNode_BinOp::PLACE_IN: m_os << "<-"; break;
        }
        m_os << " ";
        if( IS(*n.m_right, AST::ExprNode_BinOp) && dynamic_cast<AST::ExprNode_BinOp&>(*n.m_right).m_type != n.m_type ) {
            paren_wrap(n.m_right);
        }
        else
            AST::NodeVisitor::visit(n.m_right);
    }
    virtual void visit(AST::ExprNode_UniOp& n) override {
        m_expr_root = false;
        switch(n.m_type)
        {
        case AST::ExprNode_UniOp::NEGATE:   m_os << "-";    break;
        case AST::ExprNode_UniOp::INVERT:   m_os << "!";    break;
        case AST::ExprNode_UniOp::BOX:      m_os << "box ";    break;
        case AST::ExprNode_UniOp::REF:    m_os << "&";    break;
        case AST::ExprNode_UniOp::REFMUT: m_os << "&mut ";    break;
        case AST::ExprNode_UniOp::QMARK: break;
        }

        if( IS(*n.m_value, AST::ExprNode_BinOp) )
            m_os << "(";
        AST::NodeVisitor::visit(n.m_value);
        if( IS(*n.m_value, AST::ExprNode_BinOp) )
            m_os << ")";
        switch(n.m_type)
        {
        case AST::ExprNode_UniOp::QMARK: m_os << "?"; break;
        default:    break;
        }
    }


private:
    void paren_wrap(::std::unique_ptr<AST::ExprNode>& node) {
        m_os << "(";
        AST::NodeVisitor::visit(node);
        m_os << ")";
    }

    void print_attrs(const AST::AttributeList& attrs);
    void print_params(const AST::GenericParams& params);
    void print_bounds(const AST::GenericParams& params);
    void print_pattern_tuple(const AST::Pattern::TuplePat& v, bool is_refutable);
    void print_pattern(const AST::Pattern& p, bool is_refutable);
    void print_type(const TypeRef& t);

    void inc_indent();
    RepeatLitStr indent();
    void dec_indent();
};

void Dump_Rust(const char *filename, const AST::Crate& crate)
{
    ::std::ofstream os(filename);
    RustPrinter printer(os);
    printer.handle_module(crate.root_module());
}

void RustPrinter::print_attrs(const AST::AttributeList& attrs)
{
    for(const auto& a : attrs.m_items)
    {
        m_os << indent() << "#[" << a << "]\n";
    }
}

void RustPrinter::handle_module(const AST::Module& mod)
{
    bool need_nl = true;

    for( const auto& i : mod.items() )
    {
        if( !i.data.is_Use() )  continue ;
        const auto& i_data = i.data.as_Use();
        //if(need_nl) {
        //    m_os << "\n";
        //    need_nl = false;
        //}
        if( i_data.path == AST::Path() ) {
            continue ;
        }
        m_os << indent() << (i.is_pub ? "pub " : "") << "use " << i_data;
        if( i.name == "" )
        {
            m_os << "::*";
        }
        else if( i_data.path.nodes().back().name() != i.name )
        {
            m_os << " as " << i.name;
        }
        m_os << ";\n";
    }
    need_nl = true;

    for( const auto& item : mod.items() )
    {
        if( !item.data.is_Crate() )    continue ;
        const auto& e = item.data.as_Crate();

        print_attrs(item.data.attrs);
        m_os << indent() << "extern crate \"" << e.name << "\" as " << item.name << ";\n";
    }

    for( const auto& item : mod.items() )
    {
        if( !item.data.is_ExternBlock() )    continue ;
        const auto& e = item.data.as_ExternBlock();

        print_attrs(item.data.attrs);
        m_os << indent() << "extern \"" << e.abi() << "\" {}\n";
    }

    for( const auto& item : mod.items() )
    {
        if( !item.data.is_Module() )    continue ;
        const auto& e = item.data.as_Module();

        m_os << "\n";
        m_os << indent() << (item.is_pub ? "pub " : "") << "mod " << item.name << "\n";
        m_os << indent() << "{\n";
        inc_indent();
        handle_module(e);
        dec_indent();
        m_os << indent() << "}\n";
        m_os << "\n";
    }

    for( const auto& item : mod.items() )
    {
        if( !item.data.is_Type() )    continue ;
        const auto& e = item.data.as_Type();

        if(need_nl) {
            m_os << "\n";
            need_nl = false;
        }
        print_attrs(item.data.attrs);
        m_os << indent() << (item.is_pub ? "pub " : "") << "type " << item.name;
        print_params(e.params());
        m_os << " = " << e.type();
        print_bounds(e.params());
        m_os << ";\n";
    }
    need_nl = true;

    for( const auto& item : mod.items() )
    {
        if( !item.data.is_Struct() )    continue ;
        const auto& e = item.data.as_Struct();

        m_os << "\n";
        print_attrs(item.data.attrs);
        m_os << indent() << (item.is_pub ? "pub " : "") << "struct " << item.name;
        handle_struct(e);
    }

    for( const auto& item : mod.items() )
    {
        if( !item.data.is_Enum() )    continue ;
        const auto& e = item.data.as_Enum();

        m_os << "\n";
        print_attrs(item.data.attrs);
        m_os << indent() << (item.is_pub ? "pub " : "") << "enum " << item.name;
        handle_enum(e);
    }

    for( const auto& item : mod.items() )
    {
        if( !item.data.is_Trait() )    continue ;
        const auto& e = item.data.as_Trait();

        m_os << "\n";
        print_attrs(item.data.attrs);
        m_os << indent() << (item.is_pub ? "pub " : "") << "trait " << item.name;
        handle_trait(e);
    }

    for( const auto& item : mod.items() )
    {
        if( !item.data.is_Static() )    continue ;
        const auto& e = item.data.as_Static();

        if(need_nl) {
            m_os << "\n";
            need_nl = false;
        }
        print_attrs(item.data.attrs);
        m_os << indent() << (item.is_pub ? "pub " : "");
        switch( e.s_class() )
        {
        case AST::Static::CONST:  m_os << "const ";   break;
        case AST::Static::STATIC: m_os << "static ";   break;
        case AST::Static::MUT:    m_os << "static mut ";   break;
        }
        m_os << item.name << ": " << e.type() << " = ";
        e.value().visit_nodes(*this);
        m_os << ";\n";
    }

    for( const auto& item : mod.items() )
    {
        if( !item.data.is_Function() )    continue ;
        const auto& e = item.data.as_Function();

        m_os << "\n";
        print_attrs(item.data.attrs);
        handle_function(item.is_pub, item.name, e);
    }

    for( const auto& item : mod.items() )
    {
        if( !item.data.is_Impl() )    continue ;
        const auto& i = item.data.as_Impl();

        m_os << "\n";
        m_os << indent() << "impl";
        print_params(i.def().params());
        if( i.def().trait().ent != AST::Path() )
        {
                m_os << " " << i.def().trait().ent << " for";
        }
        m_os << " " << i.def().type() << "\n";

        print_bounds(i.def().params());
        m_os << indent() << "{\n";
        inc_indent();
        for( const auto& it : i.items() )
        {
            TU_MATCH_DEF(AST::Item, (*it.data), (e),
            (
                throw ::std::runtime_error(FMT("Unexpected item type in impl block - " << it.data->tag_str()));
                ),
            (None,
                // Ignore, it's been deleted by #[cfg]
                ),
            (MacroInv,
                // TODO: Dump macro invocations
                ),
            (Static,
                m_os << indent();
                switch(e.s_class())
                {
                case ::AST::Static::CONST:  m_os << "const ";   break;
                case ::AST::Static::STATIC: m_os << "static ";  break;
                case ::AST::Static::MUT:    m_os << "static mut ";  break;
                }
                m_os << it.name << ": " << e.type() << " = ";
                e.value().visit_nodes(*this);
                m_os << ";\n";
                ),
            (Type,
                m_os << indent() << "type " << it.name << " = " << e.type() << ";\n";
                ),
            (Function,
                handle_function(it.is_pub, it.name, e);
                )
            )
        }
        dec_indent();
        m_os << indent() << "}\n";
    }
}

void RustPrinter::print_params(const AST::GenericParams& params)
{
    if( params.ty_params().size() > 0 || params.lft_params().size() > 0 )
    {
        bool is_first = true;
        m_os << "<";
        // Lifetimes
        for( const auto& p : params.lft_params() )
        {
            if( !is_first )
                m_os << ", ";
            m_os << "'" << p;
            is_first = false;
        }
        // Types
        for( const auto& p : params.ty_params() )
        {
            if( !is_first )
                m_os << ", ";
            m_os << p.name();
            if( !p.get_default().is_wildcard() )
                m_os << " = " << p.get_default();
            is_first = false;
        }
        m_os << ">";
    }
}

void RustPrinter::print_bounds(const AST::GenericParams& params)
{
    if( params.bounds().size() )
    {
        m_os << indent() << "where\n";
        inc_indent();
        bool is_first = true;

        for( const auto& b : params.bounds() )
        {
            if( !is_first )
                m_os << ",\n";
            is_first = false;

            m_os << indent();
            TU_MATCH(AST::GenericBound, (b), (ent),
            (None,
                m_os << "/*-*/";
                ),
            (Lifetime,
                m_os << "'" << ent.test << ": '" << ent.bound;
                ),
            (TypeLifetime,
                m_os << ent.type << ": '" << ent.bound;
                ),
            (IsTrait,
                m_os << ent.outer_hrbs << ent.type << ": " << ent.inner_hrbs << ent.trait;
                ),
            (MaybeTrait,
                m_os << ent.type << ": ?" << ent.trait;
                ),
            (NotTrait,
                m_os << ent.type << ": !" << ent.trait;
                ),
            (Equality,
                m_os << ent.type << ": =" << ent.replacement;
                )
            )
        }
        m_os << "\n";

        dec_indent();
    }
}

void RustPrinter::print_pattern_tuple(const AST::Pattern::TuplePat& v, bool is_refutable)
{
    for(const auto& sp : v.start) {
        print_pattern(sp, is_refutable);
        m_os << ", ";
    }
    if( v.has_wildcard )
    {
        m_os << ".., ";
        for(const auto& sp : v.end) {
            print_pattern(sp, is_refutable);
            m_os << ", ";
        }
    }
}
void RustPrinter::print_pattern(const AST::Pattern& p, bool is_refutable)
{
    if( p.binding().is_valid() ) {
        if( p.binding().m_mutable )
            m_os << "mut ";
        switch(p.binding().m_type)
        {
        case ::AST::PatternBinding::Type::MOVE:
            break;
        case ::AST::PatternBinding::Type::REF:
            m_os << "ref ";
            break;
        case ::AST::PatternBinding::Type::MUTREF:
            m_os << "ref mut ";
            break;
        }
        m_os << p.binding().m_name << "/*"<<p.binding().m_slot<<"*/";
        // If binding is irrefutable, and would be binding against a wildcard, just emit the name
        if( !is_refutable && p.data().is_Any() )
        {
            return ;
        }
        m_os << " @ ";
    }
    TU_MATCH(AST::Pattern::Data, (p.data()), (v),
    (Any,
        m_os << "_";
        ),
    (MaybeBind,
        m_os << "_ /*?*/";
        ),
    (Macro,
        m_os << *v.inv;
        ),
    (Box, {
        const auto& v = p.data().as_Box();
        m_os << "& ";
        print_pattern(*v.sub, is_refutable);
        }),
    (Ref, {
        const auto& v = p.data().as_Ref();
        if(v.mut)
            m_os << "&mut ";
        else
            m_os << "& ";
        print_pattern(*v.sub, is_refutable);
        }),
    (Value,
        m_os << v.start;
        if( ! v.end.is_Invalid() ) {
            m_os << " ... " << v.end;
        }
        ),
    (StructTuple,
        m_os << v.path << "(";
        this->print_pattern_tuple(v.tup_pat, is_refutable);
        m_os << ")";
        ),
    (Struct, {
        const auto& v = p.data().as_Struct();
        m_os << v.path << "(";
        for(const auto& sp : v.sub_patterns) {
            m_os << sp.first << ": ";
            print_pattern(sp.second, is_refutable);
            m_os << ",";
        }
        m_os << ")";
        }),
    (Tuple,
        m_os << "(";
        this->print_pattern_tuple(v, is_refutable);
        m_os << ")";
        ),
    (Slice,
        m_os << "[";
        m_os << v.sub_pats;
        m_os << "]";
        ),
    (SplitSlice,
        m_os << "[";
        bool needs_comma = false;
        if(v.leading.size()) {
            m_os << v.leading;
            m_os << ", ";
        }

        if(v.extra_bind.is_valid())
        {
            const auto& b = v.extra_bind;
            if( b.m_mutable )
                m_os << "mut ";
            switch(b.m_type)
            {
            case ::AST::PatternBinding::Type::MOVE:
                break;
            case ::AST::PatternBinding::Type::REF:
                m_os << "ref ";
                break;
            case ::AST::PatternBinding::Type::MUTREF:
                m_os << "ref mut ";
                break;
            }
            m_os << b.m_name << "/*"<<b.m_slot<<"*/";
        }
        m_os << "..";
        needs_comma = true;

        if(v.trailing.size()) {
            if( needs_comma ) {
                m_os << ", ";
            }
            m_os << v.trailing;
        }
        m_os << "]";
        )
    )
}

void RustPrinter::print_type(const TypeRef& t)
{
    m_os << t;
}

void RustPrinter::handle_struct(const AST::Struct& s)
{
    print_params(s.params());

    TU_MATCH(AST::StructData, (s.m_data), (e),
    (Unit,
        m_os << " /* unit-like */\n";
        print_bounds(s.params());
        m_os << indent() << ";\n";
        ),
    (Tuple,
        m_os << "(";
        for( const auto& i : e.ents )
            m_os << i.m_type << ", ";
        m_os << ")\n";
        print_bounds(s.params());
        m_os << indent() << ";\n";
        ),
    (Struct,
        m_os << "\n";
        print_bounds(s.params());

        m_os << indent() << "{\n";
        inc_indent();
        for( const auto& i : e.ents )
        {
            m_os << indent() << (i.m_is_public ? "pub " : "") << i.m_name << ": " << i.m_type.print_pretty() << "\n";
        }
        dec_indent();
        m_os << indent() << "}\n";
        )
    )
    m_os << "\n";
}

void RustPrinter::handle_enum(const AST::Enum& s)
{
    print_params(s.params());
    m_os << "\n";
    print_bounds(s.params());

    m_os << indent() << "{\n";
    inc_indent();
    unsigned int idx = 0;
    for( const auto& i : s.variants() )
    {
        m_os << indent() << "/*"<<idx<<"*/" << i.m_name;
        TU_MATCH(AST::EnumVariantData, (i.m_data), (e),
        (Value,
            m_os << " = " << e.m_value;
            ),
        (Tuple,
            m_os << "(";
            for( const auto& t : e.m_sub_types )
                m_os << t.print_pretty() << ", ";
            m_os << ")";
            ),
        (Struct,
            m_os << "{\n";
            inc_indent();
            for( const auto& i : e.m_fields )
            {
                m_os << indent() << i.m_name << ": " << i.m_type.print_pretty() << "\n";
            }
            dec_indent();
            m_os << indent() << "}";
            )
        )
        m_os << ",\n";
        idx ++;
    }
    dec_indent();
    m_os << indent() << "}\n";
    m_os << "\n";
}

void RustPrinter::handle_trait(const AST::Trait& s)
{
    print_params(s.params());
    m_os << "\n";
    print_bounds(s.params());

    m_os << indent() << "{\n";
    inc_indent();

    for( const auto& i : s.items() )
    {
        TU_MATCH_DEF(AST::Item, (i.data), (e),
        (
            ),
        (Type,
            m_os << indent() << "type " << i.name << ";\n";
            ),
        (Function,
            handle_function(false, i.name, e);
            )
        )
    }

    dec_indent();
    m_os << indent() << "}\n";
    m_os << "\n";
}

void RustPrinter::handle_function(bool is_pub, const ::std::string& name, const AST::Function& f)
{
    m_os << indent();
    m_os << (is_pub ? "pub " : "");
    if( f.is_const() )
        m_os << "const ";
    if( f.is_unsafe() )
        m_os << "unsafe ";
    if( f.abi() != ABI_RUST )
        m_os << "extern \"" << f.abi() << "\" ";
    m_os << "fn " << name;
    print_params(f.params());
    m_os << "(";
    bool is_first = true;
    for( const auto& a : f.args() )
    {
        if( !is_first )
            m_os << ", ";
        print_pattern( a.first, false );
        m_os << ": " << a.second.print_pretty();
        is_first = false;
    }
    m_os << ")";
    if( !f.rettype().is_unit() )
    {
        m_os << " -> " << f.rettype().print_pretty();
    }

    if( f.code().is_valid() )
    {
        m_os << "\n";
        print_bounds(f.params());

        m_os << indent();
        f.code().visit_nodes(*this);
        m_os << "\n";
        //m_os << indent() << f.data.code() << "\n";
    }
    else
    {
        print_bounds(f.params());
        m_os << ";\n";
    }
}

void RustPrinter::inc_indent()
{
    m_indent_level ++;
}
RepeatLitStr RustPrinter::indent()
{
    return RepeatLitStr { "    ", m_indent_level };
}
void RustPrinter::dec_indent()
{
    m_indent_level --;
}
