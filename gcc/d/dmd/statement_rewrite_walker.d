/**
 * Provides a visitor for statements that allows rewriting the currently visited node.
 *
 * Copyright:   Copyright (C) 1999-2024 by The D Language Foundation, All Rights Reserved
 * Authors:     $(LINK2 https://www.digitalmars.com, Walter Bright)
 * License:     $(LINK2 https://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source:      $(LINK2 https://github.com/dlang/dmd/blob/master/src/dmd/statement_rewrite_walker.d, _statement_rewrite_walker.d)
 * Documentation:  https://dlang.org/phobos/dmd_statement_rewrite_walker.html
 * Coverage:    https://codecov.io/gh/dlang/dmd/src/master/src/dmd/statement_rewrite_walker.d
 */

module dmd.statement_rewrite_walker;

import core.stdc.stdio;

import dmd.statement;
import dmd.visitor;


/** A visitor to walk entire statements and provides ability to replace any sub-statements.
 */
extern (C++) class StatementRewriteWalker : SemanticTimePermissiveVisitor
{
    alias visit = SemanticTimePermissiveVisitor.visit;

    /* Point the currently visited statement.
     * By using replaceCurrent() method, you can replace AST during walking.
     */
    Statement* ps;

public:
    final void visitStmt(ref Statement s)
    {
        ps = &s;
        s.accept(this);
    }

    final void replaceCurrent(Statement s)
    {
        *ps = s;
    }

    override void visit(PeelStatement s)
    {
        if (s.s)
            visitStmt(s.s);
    }

    override void visit(CompoundStatement s)
    {
        if (s.statements && s.statements.length)
        {
            for (size_t i = 0; i < s.statements.length; i++)
            {
                if ((*s.statements)[i])
                    visitStmt((*s.statements)[i]);
            }
        }
    }

    override void visit(CompoundDeclarationStatement s)
    {
        visit(cast(CompoundStatement)s);
    }

    override void visit(UnrolledLoopStatement s)
    {
        if (s.statements && s.statements.length)
        {
            for (size_t i = 0; i < s.statements.length; i++)
            {
                if ((*s.statements)[i])
                    visitStmt((*s.statements)[i]);
            }
        }
    }

    override void visit(ScopeStatement s)
    {
        if (s.statement)
            visitStmt(s.statement);
    }

    override void visit(WhileStatement s)
    {
        if (s._body)
            visitStmt(s._body);
    }

    override void visit(DoStatement s)
    {
        if (s._body)
            visitStmt(s._body);
    }

    override void visit(ForStatement s)
    {
        if (s._init)
            visitStmt(s._init);
        if (s._body)
            visitStmt(s._body);
    }

    override void visit(ForeachStatement s)
    {
        if (s._body)
            visitStmt(s._body);
    }

    override void visit(ForeachRangeStatement s)
    {
        if (s._body)
            visitStmt(s._body);
    }

    override void visit(IfStatement s)
    {
        if (s.ifbody)
            visitStmt(s.ifbody);
        if (s.elsebody)
            visitStmt(s.elsebody);
    }

    override void visit(SwitchStatement s)
    {
        if (s._body)
            visitStmt(s._body);
    }

    override void visit(CaseStatement s)
    {
        if (s.statement)
            visitStmt(s.statement);
    }

    override void visit(CaseRangeStatement s)
    {
        if (s.statement)
            visitStmt(s.statement);
    }

    override void visit(DefaultStatement s)
    {
        if (s.statement)
            visitStmt(s.statement);
    }

    override void visit(SynchronizedStatement s)
    {
        if (s._body)
            visitStmt(s._body);
    }

    override void visit(WithStatement s)
    {
        if (s._body)
            visitStmt(s._body);
    }

    override void visit(TryCatchStatement s)
    {
        if (s._body)
            visitStmt(s._body);
        if (s.catches && s.catches.length)
        {
            for (size_t i = 0; i < s.catches.length; i++)
            {
                Catch c = (*s.catches)[i];
                if (c && c.handler)
                    visitStmt(c.handler);
            }
        }
    }

    override void visit(TryFinallyStatement s)
    {
        if (s._body)
            visitStmt(s._body);
        if (s.finalbody)
            visitStmt(s.finalbody);
    }

    override void visit(DebugStatement s)
    {
        if (s.statement)
            visitStmt(s.statement);
    }

    override void visit(LabelStatement s)
    {
        if (s.statement)
            visitStmt(s.statement);
    }
}
