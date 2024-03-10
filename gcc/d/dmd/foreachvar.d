/**
 * Utility to visit every variable in an expression.
 *
 * Copyright:   Copyright (C) 1999-2023 by The D Language Foundation, All Rights Reserved
 * Authors:     $(LINK2 https://www.digitalmars.com, Walter Bright)
 * License:     $(LINK2 https://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source:      $(LINK2 https://github.com/dlang/dmd/blob/master/src/dmd/foreachvar.d, _foreachvar.d)
 * Documentation:  https://dlang.org/phobos/dmd_foreachvar.html
 * Coverage:    https://codecov.io/gh/dlang/dmd/src/master/src/dmd/foreachvar.d
 */

module dmd.foreachvar;

import core.stdc.stdio;
import core.stdc.stdlib;
import core.stdc.string;

import dmd.arraytypes;
import dmd.astenums;
import dmd.attrib;
import dmd.dclass;
import dmd.declaration;
import dmd.dstruct;
import dmd.dsymbol;
import dmd.dsymbolsem;
import dmd.dtemplate;
import dmd.errors;
import dmd.expression;
import dmd.func;
import dmd.id;
import dmd.identifier;
import dmd.init;
import dmd.initsem;
import dmd.mtype;
import dmd.postordervisitor;
import dmd.printast;
import dmd.root.array;
import dmd.root.rootobject;
import dmd.statement;
import dmd.tokens;
import dmd.visitor;

/*********************************************
 * Visit each Expression in e, and call dgVar() on each variable declared in it.
 * Params:
 *      e = expression tree to visit
 *      dgVar = call when a variable is declared
 */
void foreachVar(Expression e, void delegate(VarDeclaration) dgVar)
{
    if (!e)
        return;

    extern (C++) final class VarWalker : StoppableVisitor
    {
        alias visit = typeof(super).visit;
        extern (D) void delegate(VarDeclaration) dgVar;

        extern (D) this(void delegate(VarDeclaration) dgVar) scope
        {
            this.dgVar = dgVar;
        }

        override void visit(Expression e)
        {
        }

        override void visit(ErrorExp e)
        {
        }

        override void visit(DeclarationExp e)
        {
            VarDeclaration v = e.declaration.isVarDeclaration();
            if (!v)
                return;
            if (TupleDeclaration td = v.toAlias().isTupleDeclaration())
                td.foreachVar((s) { dgVar(s.isVarDeclaration()); });
            else
                dgVar(v);
            Dsymbol s = v.toAlias();
            if (s == v && !v.isStatic() && v._init)
            {
                if (auto ie = v._init.isExpInitializer())
                    ie.exp.foreachVar(dgVar);
            }
        }

        override void visit(IndexExp e)
        {
            if (e.lengthVar)
                dgVar(e.lengthVar);
        }

        override void visit(SliceExp e)
        {
            if (e.lengthVar)
                dgVar(e.lengthVar);
        }
    }

    scope VarWalker v = new VarWalker(dgVar);
    walkPostorder(e, v);
}

/***************
 * Transitively walk Statement s, pass Expressions to dgExp(), VarDeclarations to dgVar().
 * Params:
 *      s = Statement to traverse
 *      dgExp = delegate to pass found Expressions to
 *      dgVar = delegate to pass found VarDeclarations to
 */
void foreachExpAndVar(Statement s,
        void delegate(Expression) dgExp,
        void delegate(VarDeclaration) dgVar)
{
    void visit(Statement s)
    {
        void visitExp(ExpStatement s)
        {
            if (s.exp)
                dgExp(s.exp);
        }

        void visitDtorExp(DtorExpStatement s)
        {
            if (s.exp)
                dgExp(s.exp);
        }

        void visitIf(IfStatement s)
        {
            dgExp(s.condition);
            visit(s.ifbody);
            visit(s.elsebody);
        }

        void visitDo(DoStatement s)
        {
            dgExp(s.condition);
            visit(s._body);
        }

        void visitFor(ForStatement s)
        {
            visit(s._init);
            if (s.condition)
                dgExp(s.condition);
            if (s.increment)
                dgExp(s.increment);
            visit(s._body);
        }

        void visitSwitch(SwitchStatement s)
        {
            dgExp(s.condition);
            // Note that the body contains the Case and Default
            // statements, so we only need to compile the expressions
            foreach (cs; *s.cases)
            {
                dgExp(cs.exp);
            }
            visit(s._body);
        }

        void visitCase(CaseStatement s)
        {
            visit(s.statement);
        }

        void visitReturn(ReturnStatement s)
        {
            if (s.exp)
                dgExp(s.exp);
        }

        void visitCompound(CompoundStatement s)
        {
            if (s.statements)
            {
                foreach (s2; *s.statements)
                {
                    visit(s2);
                }
            }
        }

        void visitCompoundDeclaration(CompoundDeclarationStatement s)
        {
            visitCompound(s);
        }

        void visitUnrolledLoop(UnrolledLoopStatement s)
        {
            foreach (s2; *s.statements)
            {
                visit(s2);
            }
        }

        void visitScope(ScopeStatement s)
        {
            visit(s.statement);
        }

        void visitDefault(DefaultStatement s)
        {
            visit(s.statement);
        }

        void visitWith(WithStatement s)
        {
            // If it is with(Enum) {...}, just execute the body.
            if (s.exp.op == EXP.scope_ || s.exp.op == EXP.type)
            {
            }
            else
            {
                dgVar(s.wthis);
                dgExp(s.exp);
            }
            visit(s._body);
        }

        void visitTryCatch(TryCatchStatement s)
        {
            visit(s._body);
            foreach (ca; *s.catches)
            {
                if (ca.var)
                    dgVar(ca.var);
                visit(ca.handler);
            }
        }

        void visitTryFinally(TryFinallyStatement s)
        {
            visit(s._body);
            visit(s.finalbody);
        }

        void visitThrow(ThrowStatement s)
        {
            dgExp(s.exp);
        }

        void visitLabel(LabelStatement s)
        {
            visit(s.statement);
        }

        if (!s)
            return;

        final switch (s.stmt)
        {
            case STMT.Exp:                 visitExp(s.isExpStatement()); break;
            case STMT.DtorExp:             visitDtorExp(s.isDtorExpStatement()); break;
            case STMT.Compound:            visitCompound(s.isCompoundStatement()); break;
            case STMT.CompoundDeclaration: visitCompoundDeclaration(s.isCompoundDeclarationStatement()); break;
            case STMT.UnrolledLoop:        visitUnrolledLoop(s.isUnrolledLoopStatement()); break;
            case STMT.Scope:               visitScope(s.isScopeStatement()); break;
            case STMT.Do:                  visitDo(s.isDoStatement()); break;
            case STMT.For:                 visitFor(s.isForStatement()); break;
            case STMT.If:                  visitIf(s.isIfStatement()); break;
            case STMT.Switch:              visitSwitch(s.isSwitchStatement()); break;
            case STMT.Case:                visitCase(s.isCaseStatement()); break;
            case STMT.Default:             visitDefault(s.isDefaultStatement()); break;
            case STMT.Return:              visitReturn(s.isReturnStatement()); break;
            case STMT.With:                visitWith(s.isWithStatement()); break;
            case STMT.TryCatch:            visitTryCatch(s.isTryCatchStatement()); break;
            case STMT.TryFinally:          visitTryFinally(s.isTryFinallyStatement()); break;
            case STMT.Throw:               visitThrow(s.isThrowStatement()); break;
            case STMT.Label:               visitLabel(s.isLabelStatement()); break;

            case STMT.CompoundAsm:
            case STMT.Asm:
            case STMT.InlineAsm:
            case STMT.GccAsm:

            case STMT.Break:
            case STMT.Continue:
            case STMT.GotoDefault:
            case STMT.GotoCase:
            case STMT.SwitchError:
            case STMT.Goto:
            case STMT.Pragma:
            case STMT.Import:
            case STMT.Error:
                break;          // ignore these

            case STMT.ScopeGuard:
            case STMT.Foreach:
            case STMT.ForeachRange:
            case STMT.Debug:
            case STMT.CaseRange:
            case STMT.StaticForeach:
            case STMT.StaticAssert:
            case STMT.Conditional:
            case STMT.While:
            case STMT.Forwarding:
            case STMT.Mixin:
            case STMT.Peel:
            case STMT.Synchronized:
                assert(0);              // should have been rewritten
        }
    }

    visit(s);
}
