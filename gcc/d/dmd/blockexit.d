/**
 * Find out in what ways control flow can exit a statement block.
 *
 * Copyright:   Copyright (C) 1999-2023 by The D Language Foundation, All Rights Reserved
 * Authors:     $(LINK2 https://www.digitalmars.com, Walter Bright)
 * License:     $(LINK2 https://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source:      $(LINK2 https://github.com/dlang/dmd/blob/master/src/dmd/blockexit.d, _blockexit.d)
 * Documentation:  https://dlang.org/phobos/dmd_blockexit.html
 * Coverage:    https://codecov.io/gh/dlang/dmd/src/master/src/dmd/blockexit.d
 */

module dmd.blockexit;

import core.stdc.stdio;

import dmd.arraytypes;
import dmd.astenums;
import dmd.canthrow;
import dmd.dclass;
import dmd.declaration;
import dmd.errorsink;
import dmd.expression;
import dmd.func;
import dmd.globals;
import dmd.id;
import dmd.identifier;
import dmd.location;
import dmd.mtype;
import dmd.statement;
import dmd.tokens;

/**
 * BE stands for BlockExit.
 *
 * It indicates if a statement does transfer control to another block.
 * A block is a sequence of statements enclosed in { }
 */
enum BE : int
{
    none      = 0,
    fallthru  = 1,
    throw_    = 2,
    return_   = 4,
    goto_     = 8,
    halt      = 0x10,
    break_    = 0x20,
    continue_ = 0x40,
    errthrow  = 0x80,
    any       = (fallthru | throw_ | return_ | goto_ | halt),
}


/*********************************************
 * Determine mask of ways that a statement can exit.
 *
 * Only valid after semantic analysis.
 * Params:
 *   s = statement to check for block exit status
 *   func = function that statement s is in
 *   eSink = generate an error if it throws
 * Returns:
 *   BE.xxxx
 */
int blockExit(Statement s, FuncDeclaration func, ErrorSink eSink)
{
        int result = BE.none;

        void visitDefaultCase(Statement s)
        {
            printf("Statement::blockExit(%p)\n", s);
            printf("%s\n", s.toChars());
            assert(0);
        }

        void visitError(ErrorStatement s)
        {
            result = BE.none;
        }

        void visitExp(ExpStatement s)
        {
            result = BE.fallthru;
            if (s.exp)
            {
                if (s.exp.op == EXP.halt)
                {
                    result = BE.halt;
                    return;
                }
                if (AssertExp a = s.exp.isAssertExp())
                {
                    if (a.e1.toBool().hasValue(false)) // if it's an assert(0)
                    {
                        result = BE.halt;
                        return;
                    }
                }
                if (s.exp.type && s.exp.type.toBasetype().isTypeNoreturn())
                    result = BE.halt;

                result |= canThrow(s.exp, func, eSink);
            }
        }

        void visitDtorExp(DtorExpStatement s)
        {
            visitExp(s);
        }

        void visitMixin(MixinStatement s)
        {
            assert(global.errors);
            result = BE.fallthru;
        }

        void visitCompound(CompoundStatement cs)
        {
            //printf("CompoundStatement.blockExit(%p) %d result = x%X\n", cs, cs.statements.length, result);
            result = BE.fallthru;
            Statement slast = null;
            foreach (s; *cs.statements)
            {
                if (s)
                {
                    //printf("result = x%x\n", result);
                    //printf("s: %s\n", s.toChars());
                    if (result & BE.fallthru && slast)
                    {
                        slast = slast.last();
                        if (slast && (slast.isCaseStatement() || slast.isDefaultStatement()) && (s.isCaseStatement() || s.isDefaultStatement()))
                        {
                            // Allow if last case/default was empty
                            CaseStatement sc = slast.isCaseStatement();
                            DefaultStatement sd = slast.isDefaultStatement();
                            auto sl = (sc ? sc.statement : (sd ? sd.statement : null));

                            if (sl && (!sl.hasCode() || sl.isErrorStatement()))
                            {
                            }
                            else if (func.getModule().filetype != FileType.c)
                            {
                                const(char)* gototype = s.isCaseStatement() ? "case" : "default";
                                // @@@DEPRECATED_2.110@@@ https://issues.dlang.org/show_bug.cgi?id=22999
                                // Deprecated in 2.100
                                // Make an error in 2.110
                                if (sl && sl.isCaseStatement())
                                    global.errorSink.deprecation(s.loc, "switch case fallthrough - use 'goto %s;' if intended", gototype);
                                else
                                    global.errorSink.error(s.loc, "switch case fallthrough - use 'goto %s;' if intended", gototype);
                            }
                        }
                    }

                    if ((result & BE.fallthru) || s.comeFrom())
                    {
                        result &= ~BE.fallthru;
                        result |= blockExit(s, func, eSink);
                    }
                    slast = s;
                }
            }
        }

        void visitUnrolledLoop(UnrolledLoopStatement uls)
        {
            result = BE.fallthru;
            foreach (s; *uls.statements)
            {
                if (s)
                {
                    int r = blockExit(s, func, eSink);
                    result |= r & ~(BE.break_ | BE.continue_ | BE.fallthru);
                    if ((r & (BE.fallthru | BE.continue_ | BE.break_)) == 0)
                        result &= ~BE.fallthru;
                }
            }
        }

        void visitScope(ScopeStatement s)
        {
            //printf("ScopeStatement::blockExit(%p)\n", s.statement);
            result = blockExit(s.statement, func, eSink);
        }

        void visitWhile(WhileStatement s)
        {
            assert(global.errors);
            result = BE.fallthru;
        }

        void visitDo(DoStatement s)
        {
            if (s._body)
            {
                result = blockExit(s._body, func, eSink);
                if (result == BE.break_)
                {
                    result = BE.fallthru;
                    return;
                }
                if (result & BE.continue_)
                    result |= BE.fallthru;
            }
            else
                result = BE.fallthru;
            if (result & BE.fallthru)
            {
                result |= canThrow(s.condition, func, eSink);

                if (!(result & BE.break_) && s.condition.toBool().hasValue(true))
                    result &= ~BE.fallthru;
            }
            result &= ~(BE.break_ | BE.continue_);
        }

        void visitFor(ForStatement s)
        {
            result = BE.fallthru;
            if (s._init)
            {
                result = blockExit(s._init, func, eSink);
                if (!(result & BE.fallthru))
                    return;
            }
            if (s.condition)
            {
                result |= canThrow(s.condition, func, eSink);

                const opt = s.condition.toBool();
                if (opt.hasValue(true))
                    result &= ~BE.fallthru;
                else if (opt.hasValue(false))
                    return;
            }
            else
                result &= ~BE.fallthru; // the body must do the exiting
            if (s._body)
            {
                int r = blockExit(s._body, func, eSink);
                if (r & (BE.break_ | BE.goto_))
                    result |= BE.fallthru;
                result |= r & ~(BE.fallthru | BE.break_ | BE.continue_);
            }
            if (s.increment)
                result |= canThrow(s.increment, func, eSink);
        }

        void visitForeach(ForeachStatement s)
        {
            result = BE.fallthru;
            result |= canThrow(s.aggr, func, eSink);

            if (s._body)
                result |= blockExit(s._body, func, eSink) & ~(BE.break_ | BE.continue_);
        }

        void visitForeachRange(ForeachRangeStatement s)
        {
            assert(global.errors);
            result = BE.fallthru;
        }

        void visitIf(IfStatement s)
        {
            //printf("IfStatement::blockExit(%p)\n", s);
            result = BE.none;
            result |= canThrow(s.condition, func, eSink);

            const opt = s.condition.toBool();
            if (opt.hasValue(true))
            {
                result |= blockExit(s.ifbody, func, eSink);
            }
            else if (opt.hasValue(false))
            {
                result |= blockExit(s.elsebody, func, eSink);
            }
            else
            {
                result |= blockExit(s.ifbody, func, eSink);
                result |= blockExit(s.elsebody, func, eSink);
            }
            //printf("IfStatement::blockExit(%p) = x%x\n", s, result);
        }

        void visitConditional(ConditionalStatement s)
        {
            result = blockExit(s.ifbody, func, eSink);
            if (s.elsebody)
                result |= blockExit(s.elsebody, func, eSink);
        }

        void visitPragma(PragmaStatement s)
        {
            result = BE.fallthru;
        }

        void visitStaticAssert(StaticAssertStatement s)
        {
            result = BE.fallthru;
        }

        void visitSwitch(SwitchStatement s)
        {
            result = BE.none;
            result |= canThrow(s.condition, func, eSink);

            if (s._body)
            {
                result |= blockExit(s._body, func, eSink);
                if (result & BE.break_)
                {
                    result |= BE.fallthru;
                    result &= ~BE.break_;
                }
            }
            else
                result |= BE.fallthru;
        }

        void visitCase(CaseStatement s)
        {
            result = blockExit(s.statement, func, eSink);
        }

        void visitDefault(DefaultStatement s)
        {
            result = blockExit(s.statement, func, eSink);
        }

        void visitGotoDefault(GotoDefaultStatement s)
        {
            result = BE.goto_;
        }

        void visitGotoCase(GotoCaseStatement s)
        {
            result = BE.goto_;
        }

        void visitSwitchError(SwitchErrorStatement s)
        {
            // Switch errors are non-recoverable
            result = BE.halt;
        }

        void visitReturn(ReturnStatement s)
        {
            result = BE.return_;
            if (s.exp)
                result |= canThrow(s.exp, func, eSink);
        }

        void visitBreak(BreakStatement s)
        {
            //printf("BreakStatement::blockExit(%p) = x%x\n", s, s.ident ? BE.goto_ : BE.break_);
            result = s.ident ? BE.goto_ : BE.break_;
        }

        void visitContinue(ContinueStatement s)
        {
            result = s.ident ? BE.continue_ | BE.goto_ : BE.continue_;
        }

        void visitSynchronized(SynchronizedStatement s)
        {
            result = blockExit(s._body, func, eSink);
        }

        void visitWith(WithStatement s)
        {
            result = BE.none;
            result |= canThrow(s.exp, func, eSink);
            result |= blockExit(s._body, func, eSink);
        }

        void visitTryCatch(TryCatchStatement s)
        {
            assert(s._body);
            result = blockExit(s._body, func, null);

            int catchresult = 0;
            foreach (c; *s.catches)
            {
                if (c.type == Type.terror)
                    continue;

                int cresult = blockExit(c.handler, func, eSink);

                /* If we're catching Object, then there is no throwing
                 */
                Identifier id = c.type.toBasetype().isClassHandle().ident;
                if (c.internalCatch && (cresult & BE.fallthru))
                {
                    // https://issues.dlang.org/show_bug.cgi?id=11542
                    // leave blockExit flags of the body
                    cresult &= ~BE.fallthru;
                }
                else if (id == Id.Object || id == Id.Throwable)
                {
                    result &= ~(BE.throw_ | BE.errthrow);
                }
                else if (id == Id.Exception)
                {
                    result &= ~BE.throw_;
                }
                catchresult |= cresult;
            }
            if (eSink && (result & BE.throw_))
            {
                // now explain why this is nothrow
                blockExit(s._body, func, eSink);
            }
            result |= catchresult;
        }

        void visitTryFinally(TryFinallyStatement s)
        {
            result = BE.fallthru;
            if (s._body)
                result = blockExit(s._body, func, null);

            // check finally body as well, it may throw (bug #4082)
            int finalresult = BE.fallthru;
            if (s.finalbody)
                finalresult = blockExit(s.finalbody, func, null);

            // If either body or finalbody halts
            if (result == BE.halt)
                finalresult = BE.none;
            if (finalresult == BE.halt)
                result = BE.none;

            if (eSink)
            {
                // now explain why this is nothrow
                if (s._body && (result & BE.throw_))
                    blockExit(s._body, func, eSink);
                if (s.finalbody && (finalresult & BE.throw_))
                    blockExit(s.finalbody, func, eSink);
            }

            if (!(finalresult & BE.fallthru))
                result &= ~BE.fallthru;
            result |= finalresult & ~BE.fallthru;
        }

        void visitScopeGuard(ScopeGuardStatement s)
        {
            // At this point, this statement is just an empty placeholder
            result = BE.fallthru;
        }

        void visitThrow(ThrowStatement s)
        {
            if (s.internalThrow)
            {
                // https://issues.dlang.org/show_bug.cgi?id=8675
                // Allow throwing 'Throwable' object even if eSink.
                result = BE.fallthru;
                return;
            }

            result = checkThrow(s.loc, s.exp, func, eSink);
        }

        void visitGoto(GotoStatement s)
        {
            //printf("GotoStatement::blockExit(%p)\n", s);
            result = BE.goto_;
        }

        void visitLabel(LabelStatement s)
        {
            //printf("LabelStatement::blockExit(%p)\n", s);
            result = blockExit(s.statement, func, eSink);
            if (s.breaks)
                result |= BE.fallthru;
        }

        void visitCompoundAsm(CompoundAsmStatement s)
        {
            // Assume the worst
            result = BE.fallthru | BE.return_ | BE.goto_ | BE.halt;
            if (!(s.stc & STC.nothrow_))
            {
                if(func)
                    func.setThrow(s.loc, "`asm` statement is assumed to throw - mark it with `nothrow` if it does not");
                if (eSink)
                    eSink.error(s.loc, "`asm` statement is assumed to throw - mark it with `nothrow` if it does not"); // TODO
                else
                    result |= BE.throw_;
            }
        }

        void visitImport(ImportStatement s)
        {
            result = BE.fallthru;
        }

    if (!s)
        return BE.fallthru;
    mixin VisitStatement!void visit;
    visit.VisitStatement(s);
    return result;
}

/++
 + Checks whether `throw <exp>` throws an `Exception` or an `Error`
 + and raises an error if this violates `nothrow`.
 +
 + Params:
 +   loc          = location of the `throw`
 +   exp          = expression yielding the throwable
 +   eSink        = if !null then inside of a `nothrow` scope
 +   func         = function containing the `throw`
 +
 + Returns: `BE.[err]throw` depending on the type of `exp`
 +/
BE checkThrow(ref const Loc loc, Expression exp, FuncDeclaration func, ErrorSink eSink)
{
    Type t = exp.type.toBasetype();
    ClassDeclaration cd = t.isClassHandle();
    assert(cd);

    if (cd.isErrorException())
    {
        return BE.errthrow;
    }
    if (eSink)
        eSink.error(loc, "`%s` is thrown but not caught", exp.type.toChars());
    else if (func)
        func.setThrow(loc, "`%s` is thrown but not caught", exp.type);

    return BE.throw_;
}
