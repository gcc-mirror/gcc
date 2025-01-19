/**
 * A visitor that facilitates the traversal of subsets of the AST.
 *
 * Documentation:  https://dlang.org/phobos/dmd_permissivevisitor.html
 * Coverage:    https://codecov.io/gh/dlang/dmd/src/master/src/dmd/permissivevisitor.d
 */

module dmd.visitor.permissive;

import dmd.visitor.parsetime;

/** PermissiveVisitor overrides all the visit methods in  the parent class
  * that assert(0) in order to facilitate the traversal of subsets of the AST.
  * It does not implement any visiting logic.
  */
extern(C++) class PermissiveVisitor(AST): ParseTimeVisitor!AST
{
    alias visit = ParseTimeVisitor!AST.visit;

    override void visit(AST.Dsymbol){}
    override void visit(AST.Parameter){}
    override void visit(AST.Statement){}
    override void visit(AST.Type){}
    override void visit(AST.Expression){}
    override void visit(AST.TemplateParameter){}
    override void visit(AST.Condition){}
    override void visit(AST.Initializer){}
}
