/**
 * Provides an AST printer for debugging.
 *
 * Copyright:   Copyright (C) 1999-2024 by The D Language Foundation, All Rights Reserved
 * Authors:     $(LINK2 https://www.digitalmars.com, Walter Bright)
 * License:     $(LINK2 https://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source:      $(LINK2 https://github.com/dlang/dmd/blob/master/src/dmd/printast.d, _printast.d)
 * Documentation:  https://dlang.org/phobos/dmd_printast.html
 * Coverage:    https://codecov.io/gh/dlang/dmd/src/master/src/dmd/printast.d
 */

module dmd.printast;

import core.stdc.stdio;

import dmd.expression;
import dmd.ctfeexpr;
import dmd.tokens;
import dmd.visitor;
import dmd.hdrgen;

/********************
 * Print AST data structure in a nice format.
 * Params:
 *  e = expression AST to print
 *  indent = indentation level
 */
void printAST(Expression e, int indent = 0)
{
    scope PrintASTVisitor pav = new PrintASTVisitor(indent);
    e.accept(pav);
}

private:

extern (C++) final class PrintASTVisitor : Visitor
{
    alias visit = Visitor.visit;

    int indent;

    extern (D) this(int indent) scope @safe
    {
        this.indent = indent;
    }

    override void visit(Expression e)
    {
        printIndent(indent);
        auto s = EXPtoString(e.op);
        printf("%.*s %s\n", cast(int)s.length, s.ptr, e.type ? e.type.toChars() : "");
    }

    override void visit(IdentifierExp e)
    {
        printIndent(indent);
        printf("Identifier `%s` %s\n", e.ident.toChars(), e.type ? e.type.toChars() : "");
    }

    override void visit(IntegerExp e)
    {
        printIndent(indent);
        printf("Integer %lld %s\n", e.toInteger(), e.type ? e.type.toChars() : "");
    }

    override void visit(RealExp e)
    {
        printIndent(indent);

        import dmd.hdrgen : floatToBuffer;
        import dmd.common.outbuffer : OutBuffer;
        OutBuffer buf;
        floatToBuffer(e.type, e.value, buf, false);
        printf("Real %s %s\n", buf.peekChars(), e.type ? e.type.toChars() : "");
    }

    override void visit(StructLiteralExp e)
    {
        printIndent(indent);
        auto s = EXPtoString(e.op);
        printf("%.*s %s, %s\n", cast(int)s.length, s.ptr, e.type ? e.type.toChars() : "", e.toChars());
    }

    override void visit(SymbolExp e)
    {
        printIndent(indent);
        printf("Symbol %s\n", e.type ? e.type.toChars() : "");
        printIndent(indent + 2);
        printf(".var: %s\n", e.var ? e.var.toChars() : "");
    }

    override void visit(SymOffExp e)
    {
        printIndent(indent);
        printf("SymOff %s\n", e.type ? e.type.toChars() : "");
        printIndent(indent + 2);
        printf(".var: %s\n", e.var ? e.var.toChars() : "");
        printIndent(indent + 2);
        printf(".offset: %llx\n", e.offset);
    }

    override void visit(VarExp e)
    {
        printIndent(indent);
        printf("Var %s\n", e.type ? e.type.toChars() : "");
        printIndent(indent + 2);
        printf(".var: %s\n", e.var ? e.var.toChars() : "");
    }

    override void visit(DsymbolExp e)
    {
        visit(cast(Expression)e);
        printIndent(indent + 2);
        printf(".s: %s\n", e.s ? e.s.toChars() : "");
    }

    override void visit(DotIdExp e)
    {
        printIndent(indent);
        printf("DotId %s\n", e.type ? e.type.toChars() : "");
        printIndent(indent + 2);
        printf(".ident: %s\n", e.ident.toChars());
        printAST(e.e1, indent + 2);
    }

    override void visit(UnaExp e)
    {
        visit(cast(Expression)e);
        printAST(e.e1, indent + 2);
    }

    override void visit(CastExp e)
    {
        printIndent(indent);
        auto s = EXPtoString(e.op);
        printf("%.*s %s\n", cast(int)s.length, s.ptr, e.type ? e.type.toChars() : "");
        printIndent(indent + 2);
        printf(".to: %s\n", e.to.toChars());
        printAST(e.e1, indent + 2);
    }

    override void visit(VectorExp e)
    {
        printIndent(indent);
        printf("Vector %s\n", e.type ? e.type.toChars() : "");
        printIndent(indent + 2);
        printf(".to: %s\n", e.to.toChars());
        printAST(e.e1, indent + 2);
    }

    override void visit(VectorArrayExp e)
    {
        printIndent(indent);
        printf("VectorArray %s\n", e.type ? e.type.toChars() : "");
        printAST(e.e1, indent + 2);
    }

    override void visit(DotVarExp e)
    {
        printIndent(indent);
        printf("DotVar %s\n", e.type ? e.type.toChars() : "");
        printIndent(indent + 2);
        printf(".var: %s\n", e.var.toChars());
        printAST(e.e1, indent + 2);
    }

    override void visit(BinExp e)
    {
        visit(cast(Expression)e);
        printAST(e.e1, indent + 2);
        printAST(e.e2, indent + 2);
    }

    override void visit(AssignExp e)
    {
        printIndent(indent);
        printf("Assign %s\n", e.type ? e.type.toChars() : "");
        printAST(e.e1, indent + 2);
        printAST(e.e2, indent + 2);
    }

    override void visit(ConstructExp e)
    {
        printIndent(indent);
        printf("Construct %s\n", e.type ? e.type.toChars() : "");
        printAST(e.e1, indent + 2);
        printAST(e.e2, indent + 2);
    }

    override void visit(BlitExp e)
    {
        printIndent(indent);
        printf("Blit %s\n", e.type ? e.type.toChars() : "");
        printAST(e.e1, indent + 2);
        printAST(e.e2, indent + 2);
    }

    override void visit(IndexExp e)
    {
        printIndent(indent);
        printf("Index %s\n", e.type ? e.type.toChars() : "");
        printAST(e.e1, indent + 2);
        printAST(e.e2, indent + 2);
    }

    override void visit(DelegateExp e)
    {
        visit(cast(Expression)e);
        printIndent(indent + 2);
        printf(".func: %s\n", e.func ? e.func.toChars() : "");
    }

    override void visit(CompoundLiteralExp e)
    {
        visit(cast(Expression)e);
        printIndent(indent + 2);
        printf(".init: %s\n", e.initializer ? e.initializer.toChars() : "");
    }

    override void visit(ClassReferenceExp e)
    {
        visit(cast(Expression)e);
        printIndent(indent + 2);
        printf(".value: %s\n", e.value ? e.value.toChars() : "");
        printAST(e.value, indent + 2);
    }

    override void visit(ArrayLiteralExp e)
    {
        visit(cast(Expression)e);
        printIndent(indent + 2);
        printf(".basis : %s\n", e.basis ? e.basis.toChars() : "");
        if (e.elements)
        {
            printIndent(indent + 2);
            printf("[");
            foreach (i, element; (*e.elements)[])
            {
                if (i)
                    printf(", ");
                printf("%s", element.toChars());
            }
            printf("]\n");
        }
    }

    static void printIndent(int indent)
    {
        foreach (i; 0 .. indent)
            putc(' ', stdout);
    }
}
