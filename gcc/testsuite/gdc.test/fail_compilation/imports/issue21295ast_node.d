module imports.issue21295ast_node;
import imports.issue21295visitor : Visitor;
class ASTNode {
    void accept(Visitor);
}
