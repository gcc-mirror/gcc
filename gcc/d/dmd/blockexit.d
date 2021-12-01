/**
 * Find out in what ways control flow can exit a statement block.
 *
 * Copyright:   Copyright (C) 1999-2021 by The D Language Foundation, All Rights Reserved
 * Authors:     $(LINK2 http://www.digitalmars.com, Walter Bright)
 * License:     $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
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
import dmd.expression;
import dmd.func;
import dmd.globals;
import dmd.id;
import dmd.identifier;
import dmd.mtype;
import dmd.statement;
import dmd.tokens;
import dmd.visitor;

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
 *   mustNotThrow = generate an error if it throws
 * Returns:
 *   BE.xxxx
 */
int blockExit(Statement s, FuncDeclaration func, bool mustNotThrow)
{
    extern (C++) final class BlockExit : Visitor
    {
        alias visit = Visitor.visit;
    public:
        FuncDeclaration func;
        bool mustNotThrow;
        int result;

        extern (D) this(FuncDeclaration func, bool mustNotThrow)
        {
            this.func = func;
            this.mustNotThrow = mustNotThrow;
            result = BE.none;
        }

        override void visit(Statement s)
        {
            printf("Statement::blockExit(%p)\n", s);
            printf("%s\n", s.toChars());
            assert(0);
        }

        override void visit(ErrorStatement s)
        {
            result = BE.none;
        }

        override void visit(ExpStatement s)
        {
            result = BE.fallthru;
            if (s.exp)
            {
                if (s.exp.op == TOK.halt)
                {
                    result = BE.halt;
                    return;
                }
                if (s.exp.op == TOK.assert_)
                {
                    AssertExp a = cast(AssertExp)s.exp;
                    if (a.e1.isBool(false)) // if it's an assert(0)
                    {
                        result = BE.halt;
                        return;
                    }
                }
                if (s.exp.type.toBasetype().isTypeNoreturn())
                    result = BE.halt;
                if (canThrow(s.exp, func, mustNotThrow))
                    result |= BE.throw_;
            }
        }

        override void visit(CompileStatement s)
        {
            assert(global.errors);
            result = BE.fallthru;
        }

        override void visit(CompoundStatement cs)
        {
            //printf("CompoundStatement.blockExit(%p) %d result = x%X\n", cs, cs.statements.dim, result);
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
                            if (sc && (!sc.statement.hasCode() || sc.statement.isCaseStatement() || sc.statement.isErrorStatement()))
                            {
                            }
                            else if (sd && (!sd.statement.hasCode() || sd.statement.isCaseStatement() || sd.statement.isErrorStatement()))
                            {
                            }
                            else
                            {
                                const(char)* gototype = s.isCaseStatement() ? "case" : "default";
                                s.deprecation("switch case fallthrough - use 'goto %s;' if intended", gototype);
                            }
                        }
                    }

                    if (!(result & BE.fallthru) && !s.comeFrom())
                    {
                        if (blockExit(s, func, mustNotThrow) != BE.halt && s.hasCode() &&
                            s.loc != Loc.initial) // don't emit warning for generated code
                            s.warning("statement is not reachable");
                    }
                    else
                    {
                        result &= ~BE.fallthru;
                        result |= blockExit(s, func, mustNotThrow);
                    }
                    slast = s;
                }
            }
        }

        override void visit(UnrolledLoopStatement uls)
        {
            result = BE.fallthru;
            foreach (s; *uls.statements)
            {
                if (s)
                {
                    int r = blockExit(s, func, mustNotThrow);
                    result |= r & ~(BE.break_ | BE.continue_ | BE.fallthru);
                    if ((r & (BE.fallthru | BE.continue_ | BE.break_)) == 0)
                        result &= ~BE.fallthru;
                }
            }
        }

        override void visit(ScopeStatement s)
        {
            //printf("ScopeStatement::blockExit(%p)\n", s.statement);
            result = blockExit(s.statement, func, mustNotThrow);
        }

        override void visit(WhileStatement s)
        {
            assert(global.errors);
            result = BE.fallthru;
        }

        override void visit(DoStatement s)
        {
            if (s._body)
            {
                result = blockExit(s._body, func, mustNotThrow);
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
                if (canThrow(s.condition, func, mustNotThrow))
                    result |= BE.throw_;
                if (!(result & BE.break_) && s.condition.isBool(true))
                    result &= ~BE.fallthru;
            }
            result &= ~(BE.break_ | BE.continue_);
        }

        override void visit(ForStatement s)
        {
            result = BE.fallthru;
            if (s._init)
            {
                result = blockExit(s._init, func, mustNotThrow);
                if (!(result & BE.fallthru))
                    return;
            }
            if (s.condition)
            {
                if (canThrow(s.condition, func, mustNotThrow))
                    result |= BE.throw_;
                if (s.condition.isBool(true))
                    result &= ~BE.fallthru;
                else if (s.condition.isBool(false))
                    return;
            }
            else
                result &= ~BE.fallthru; // the body must do the exiting
            if (s._body)
            {
                int r = blockExit(s._body, func, mustNotThrow);
                if (r & (BE.break_ | BE.goto_))
                    result |= BE.fallthru;
                result |= r & ~(BE.fallthru | BE.break_ | BE.continue_);
            }
            if (s.increment && canThrow(s.increment, func, mustNotThrow))
                result |= BE.throw_;
        }

        override void visit(ForeachStatement s)
        {
            result = BE.fallthru;
            if (canThrow(s.aggr, func, mustNotThrow))
                result |= BE.throw_;
            if (s._body)
                result |= blockExit(s._body, func, mustNotThrow) & ~(BE.break_ | BE.continue_);
        }

        override void visit(ForeachRangeStatement s)
        {
            assert(global.errors);
            result = BE.fallthru;
        }

        override void visit(IfStatement s)
        {
            //printf("IfStatement::blockExit(%p)\n", s);
            result = BE.none;
            if (canThrow(s.condition, func, mustNotThrow))
                result |= BE.throw_;
            if (s.condition.isBool(true))
            {
                result |= blockExit(s.ifbody, func, mustNotThrow);
            }
            else if (s.condition.isBool(false))
            {
                result |= blockExit(s.elsebody, func, mustNotThrow);
            }
            else
            {
                result |= blockExit(s.ifbody, func, mustNotThrow);
                result |= blockExit(s.elsebody, func, mustNotThrow);
            }
            //printf("IfStatement::blockExit(%p) = x%x\n", s, result);
        }

        override void visit(ConditionalStatement s)
        {
            result = blockExit(s.ifbody, func, mustNotThrow);
            if (s.elsebody)
                result |= blockExit(s.elsebody, func, mustNotThrow);
        }

        override void visit(PragmaStatement s)
        {
            result = BE.fallthru;
        }

        override void visit(StaticAssertStatement s)
        {
            result = BE.fallthru;
        }

        override void visit(SwitchStatement s)
        {
            result = BE.none;
            if (canThrow(s.condition, func, mustNotThrow))
                result |= BE.throw_;
            if (s._body)
            {
                result |= blockExit(s._body, func, mustNotThrow);
                if (result & BE.break_)
                {
                    result |= BE.fallthru;
                    result &= ~BE.break_;
                }
            }
            else
                result |= BE.fallthru;
        }

        override void visit(CaseStatement s)
        {
            result = blockExit(s.statement, func, mustNotThrow);
        }

        override void visit(DefaultStatement s)
        {
            result = blockExit(s.statement, func, mustNotThrow);
        }

        override void visit(GotoDefaultStatement s)
        {
            result = BE.goto_;
        }

        override void visit(GotoCaseStatement s)
        {
            result = BE.goto_;
        }

        override void visit(SwitchErrorStatement s)
        {
            // Switch errors are non-recoverable
            result = BE.halt;
        }

        override void visit(ReturnStatement s)
        {
            result = BE.return_;
            if (s.exp && canThrow(s.exp, func, mustNotThrow))
                result |= BE.throw_;
        }

        override void visit(BreakStatement s)
        {
            //printf("BreakStatement::blockExit(%p) = x%x\n", s, s.ident ? BE.goto_ : BE.break_);
            result = s.ident ? BE.goto_ : BE.break_;
        }

        override void visit(ContinueStatement s)
        {
            result = s.ident ? BE.continue_ | BE.goto_ : BE.continue_;
        }

        override void visit(SynchronizedStatement s)
        {
            result = blockExit(s._body, func, mustNotThrow);
        }

        override void visit(WithStatement s)
        {
            result = BE.none;
            if (canThrow(s.exp, func, mustNotThrow))
                result = BE.throw_;
            result |= blockExit(s._body, func, mustNotThrow);
        }

        override void visit(TryCatchStatement s)
        {
            assert(s._body);
            result = blockExit(s._body, func, false);

            int catchresult = 0;
            foreach (c; *s.catches)
            {
                if (c.type == Type.terror)
                    continue;

                int cresult = blockExit(c.handler, func, mustNotThrow);

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
            if (mustNotThrow && (result & BE.throw_))
            {
                // now explain why this is nothrow
                blockExit(s._body, func, mustNotThrow);
            }
            result |= catchresult;
        }

        override void visit(TryFinallyStatement s)
        {
            result = BE.fallthru;
            if (s._body)
                result = blockExit(s._body, func, false);

            // check finally body as well, it may throw (bug #4082)
            int finalresult = BE.fallthru;
            if (s.finalbody)
                finalresult = blockExit(s.finalbody, func, false);

            // If either body or finalbody halts
            if (result == BE.halt)
                finalresult = BE.none;
            if (finalresult == BE.halt)
                result = BE.none;

            if (mustNotThrow)
            {
                // now explain why this is nothrow
                if (s._body && (result & BE.throw_))
                    blockExit(s._body, func, mustNotThrow);
                if (s.finalbody && (finalresult & BE.throw_))
                    blockExit(s.finalbody, func, mustNotThrow);
            }

            version (none)
            {
                // https://issues.dlang.org/show_bug.cgi?id=13201
                // Mask to prevent spurious warnings for
                // destructor call, exit of synchronized statement, etc.
                if (result == BE.halt && finalresult != BE.halt && s.finalbody && s.finalbody.hasCode())
                {
                    s.finalbody.warning("statement is not reachable");
                }
            }

            if (!(finalresult & BE.fallthru))
                result &= ~BE.fallthru;
            result |= finalresult & ~BE.fallthru;
        }

        override void visit(ScopeGuardStatement s)
        {
            // At this point, this statement is just an empty placeholder
            result = BE.fallthru;
        }

        override void visit(ThrowStatement s)
        {
            if (s.internalThrow)
            {
                // https://issues.dlang.org/show_bug.cgi?id=8675
                // Allow throwing 'Throwable' object even if mustNotThrow.
                result = BE.fallthru;
                return;
            }

            Type t = s.exp.type.toBasetype();
            ClassDeclaration cd = t.isClassHandle();
            assert(cd);

            if (cd == ClassDeclaration.errorException || ClassDeclaration.errorException.isBaseOf(cd, null))
            {
                result = BE.errthrow;
                return;
            }
            if (mustNotThrow)
                s.error("`%s` is thrown but not caught", s.exp.type.toChars());

            result = BE.throw_;
        }

        override void visit(GotoStatement s)
        {
            //printf("GotoStatement::blockExit(%p)\n", s);
            result = BE.goto_;
        }

        override void visit(LabelStatement s)
        {
            //printf("LabelStatement::blockExit(%p)\n", s);
            result = blockExit(s.statement, func, mustNotThrow);
            if (s.breaks)
                result |= BE.fallthru;
        }

        override void visit(CompoundAsmStatement s)
        {
            // Assume the worst
            result = BE.fallthru | BE.return_ | BE.goto_ | BE.halt;
            if (!(s.stc & STC.nothrow_))
            {
                if (mustNotThrow && !(s.stc & STC.nothrow_))
                    s.deprecation("`asm` statement is assumed to throw - mark it with `nothrow` if it does not");
                else
                    result |= BE.throw_;
            }
        }

        override void visit(ImportStatement s)
        {
            result = BE.fallthru;
        }
    }

    if (!s)
        return BE.fallthru;
    scope BlockExit be = new BlockExit(func, mustNotThrow);
    s.accept(be);
    return be.result;
}

