/*
  * MRustC - Mutabah's Rust Compiler
 * - By John Hodge (Mutabah/thePowersGang)
 *
 * ast/expr_ptr.hpp
 * - Pointer type wrapping AST::ExprNode (prevents need to know the full definition)
 */
#include <memory>

namespace AST {

class ExprNode;
class NodeVisitor;

typedef ::std::unique_ptr<AST::ExprNode>    ExprNodeP;
extern ::std::ostream& operator<<(::std::ostream& os, const ExprNode& node);

class Expr
{
    ::std::shared_ptr<ExprNode> m_node;
public:
    Expr(unique_ptr<ExprNode> node);
    Expr(ExprNode* node);
    Expr();

    bool is_valid() const { return m_node.get() != nullptr; }
    const ExprNode& node() const { assert(m_node.get()); return *m_node; }
          ExprNode& node()       { assert(m_node.get()); return *m_node; }
    ::std::shared_ptr<ExprNode> take_node() { assert(m_node.get()); return ::std::move(m_node); }
    void visit_nodes(NodeVisitor& v);
    void visit_nodes(NodeVisitor& v) const;

    Expr clone() const;

    friend ::std::ostream& operator<<(::std::ostream& os, const Expr& pat);
};

}
