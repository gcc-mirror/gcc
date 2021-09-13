
/* Compiler implementation of the D programming language
 * Copyright (C) 1999-2021 by The D Language Foundation, All Rights Reserved
 * written by Walter Bright
 * http://www.digitalmars.com
 * Distributed under the Boost Software License, Version 1.0.
 * http://www.boost.org/LICENSE_1_0.txt
 */

#include "statement.h"
#include "visitor.h"

/* A visitor to walk entire statements and provides ability to replace any sub-statements.
 */
class StatementRewriteWalker : public Visitor
{
    /* Point the currently visited statement.
     * By using replaceCurrent() method, you can replace AST during walking.
     */
    Statement **ps;
public:
    void visitStmt(Statement *&s) { ps = &s; s->accept(this); }
    void replaceCurrent(Statement *s) { *ps = s; }

    void visit(ErrorStatement *) {  }
    void visit(PeelStatement *s)
    {
        if (s->s)
            visitStmt(s->s);
    }
    void visit(ExpStatement *) {  }
    void visit(DtorExpStatement *) {  }
    void visit(CompileStatement *) {  }
    void visit(CompoundStatement *s)
    {
        if (s->statements && s->statements->length)
        {
            for (size_t i = 0; i < s->statements->length; i++)
            {
                if ((*s->statements)[i])
                    visitStmt((*s->statements)[i]);
            }
        }
    }
    void visit(CompoundDeclarationStatement *s) { visit((CompoundStatement *)s); }
    void visit(UnrolledLoopStatement *s)
    {
        if (s->statements && s->statements->length)
        {
            for (size_t i = 0; i < s->statements->length; i++)
            {
                if ((*s->statements)[i])
                    visitStmt((*s->statements)[i]);
            }
        }
    }
    void visit(ScopeStatement *s)
    {
        if (s->statement)
            visitStmt(s->statement);
    }
    void visit(WhileStatement *s)
    {
        if (s->_body)
            visitStmt(s->_body);
    }
    void visit(DoStatement *s)
    {
        if (s->_body)
            visitStmt(s->_body);
    }
    void visit(ForStatement *s)
    {
        if (s->_init)
            visitStmt(s->_init);
        if (s->_body)
            visitStmt(s->_body);
    }
    void visit(ForeachStatement *s)
    {
        if (s->_body)
            visitStmt(s->_body);
    }
    void visit(ForeachRangeStatement *s)
    {
        if (s->_body)
            visitStmt(s->_body);
    }
    void visit(IfStatement *s)
    {
        if (s->ifbody)
            visitStmt(s->ifbody);
        if (s->elsebody)
            visitStmt(s->elsebody);
    }
    void visit(ConditionalStatement *) {  }
    void visit(PragmaStatement *) {  }
    void visit(StaticAssertStatement *) {  }
    void visit(SwitchStatement *s)
    {
        if (s->_body)
            visitStmt(s->_body);
    }
    void visit(CaseStatement *s)
    {
        if (s->statement)
            visitStmt(s->statement);
    }
    void visit(CaseRangeStatement *s)
    {
        if (s->statement)
            visitStmt(s->statement);
    }
    void visit(DefaultStatement *s)
    {
        if (s->statement)
            visitStmt(s->statement);
    }
    void visit(GotoDefaultStatement *) {  }
    void visit(GotoCaseStatement *) {  }
    void visit(SwitchErrorStatement *) {  }
    void visit(ReturnStatement *) {  }
    void visit(BreakStatement *) {  }
    void visit(ContinueStatement *) {  }
    void visit(SynchronizedStatement *s)
    {
        if (s->_body)
            visitStmt(s->_body);
    }
    void visit(WithStatement *s)
    {
        if (s->_body)
            visitStmt(s->_body);
    }
    void visit(TryCatchStatement *s)
    {
        if (s->_body)
            visitStmt(s->_body);
        if (s->catches && s->catches->length)
        {
            for (size_t i = 0; i < s->catches->length; i++)
            {
                Catch *c = (*s->catches)[i];
                if (c && c->handler)
                    visitStmt(c->handler);
            }
        }
    }
    void visit(TryFinallyStatement *s)
    {
        if (s->_body)
            visitStmt(s->_body);
        if (s->finalbody)
            visitStmt(s->finalbody);
    }
    void visit(ScopeGuardStatement *) {  }
    void visit(ThrowStatement *) {  }
    void visit(DebugStatement *s)
    {
        if (s->statement)
            visitStmt(s->statement);
    }
    void visit(GotoStatement *) {  }
    void visit(LabelStatement *s)
    {
        if (s->statement)
            visitStmt(s->statement);
    }
    void visit(AsmStatement *) {  }
    void visit(ImportStatement *) {  }
};

