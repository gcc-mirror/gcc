/* TEST_OUTPUT:
---
fail_compilation/fail18093.d(20): Error: function `void fail18093.GenericTransitiveVisitor!(ASTCodegen).GenericTransitiveVisitor.ParseVisitMethods!(ASTCodegen).visit()` does not override any function
fail_compilation/fail18093.d(16):        did you mean to override `extern (C++) void fail18093.ParseTimeVisitor!(ASTCodegen).ParseTimeVisitor.visit()`?
fail_compilation/fail18093.d(25): Error: mixin `fail18093.GenericTransitiveVisitor!(ASTCodegen).GenericTransitiveVisitor.ParseVisitMethods!(ASTCodegen)` error instantiating
fail_compilation/fail18093.d(28): Error: template instance `fail18093.GenericTransitiveVisitor!(ASTCodegen)` error instantiating
---
 * https://issues.dlang.org/show_bug.cgi?id=18093
 */


struct ASTCodegen {}

extern (C++) class ParseTimeVisitor(AST)
{
    void visit() {}
}
template ParseVisitMethods(AST)
{
    override void visit() {}
}

class GenericTransitiveVisitor(AST) : ParseTimeVisitor!AST
{
    mixin ParseVisitMethods!AST;
}

alias SemanticTimeTransitiveVisitor = GenericTransitiveVisitor!ASTCodegen;
