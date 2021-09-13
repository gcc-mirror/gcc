
/* Compiler implementation of the D programming language
 * Copyright (C) 1999-2021 by The D Language Foundation, All Rights Reserved
 * written by Walter Bright
 * http://www.digitalmars.com
 * Distributed under the Boost Software License, Version 1.0.
 * http://www.boost.org/LICENSE_1_0.txt
 */

#include "statement.h"
#include "declaration.h"
#include "aggregate.h"
#include "id.h"

/* Only valid after semantic analysis
 * If 'mustNotThrow' is true, generate an error if it throws
 */
int blockExit(Statement *s, FuncDeclaration *func, bool mustNotThrow)
{
    class BlockExit : public Visitor
    {
    public:
        FuncDeclaration *func;
        bool mustNotThrow;
        int result;

        BlockExit(FuncDeclaration *func, bool mustNotThrow)
            : func(func), mustNotThrow(mustNotThrow)
        {
            result = BEnone;
        }

        void visit(Statement *s)
        {
            printf("Statement::blockExit(%p)\n", s);
            printf("%s\n", s->toChars());
            assert(0);
            result = BEany;
        }

        void visit(ErrorStatement *)
        {
            result = BEany;
        }

        void visit(ExpStatement *s)
        {
            result = BEfallthru;
            if (s->exp)
            {
                if (s->exp->op == TOKhalt)
                {
                    result = BEhalt;
                    return;
                }
                if (s->exp->op == TOKassert)
                {
                    AssertExp *a = (AssertExp *)s->exp;
                    if (a->e1->isBool(false))   // if it's an assert(0)
                    {
                        result = BEhalt;
                        return;
                    }
                }
                if (s->exp->type->toBasetype()->isTypeNoreturn())
                    result = BEhalt;
                if (canThrow(s->exp, func, mustNotThrow))
                    result |= BEthrow;
            }
        }

        void visit(CompileStatement *)
        {
            assert(global.errors);
            result = BEfallthru;
        }

        void visit(CompoundStatement *cs)
        {
            //printf("CompoundStatement::blockExit(%p) %d result = x%X\n", cs, cs->statements->length, result);
            result = BEfallthru;
            Statement *slast = NULL;
            for (size_t i = 0; i < cs->statements->length; i++)
            {
                Statement *s = (*cs->statements)[i];
                if (s)
                {
                    //printf("result = x%x\n", result);
                    //printf("s: %s\n", s->toChars());
                    if (result & BEfallthru && slast)
                    {
                        slast = slast->last();
                        if (slast && (slast->isCaseStatement() || slast->isDefaultStatement()) &&
                                     (s->isCaseStatement() || s->isDefaultStatement()))
                        {
                            // Allow if last case/default was empty
                            CaseStatement *sc = slast->isCaseStatement();
                            DefaultStatement *sd = slast->isDefaultStatement();
                            if (sc && (!sc->statement->hasCode() || sc->statement->isCaseStatement() || sc->statement->isErrorStatement()))
                                ;
                            else if (sd && (!sd->statement->hasCode() || sd->statement->isCaseStatement() || sd->statement->isErrorStatement()))
                                ;
                            else
                            {
                                const char *gototype = s->isCaseStatement() ? "case" : "default";
                                s->deprecation("switch case fallthrough - use 'goto %s;' if intended", gototype);
                            }
                        }
                    }

                    if (!(result & BEfallthru) && !s->comeFrom())
                    {
                        if (blockExit(s, func, mustNotThrow) != BEhalt && s->hasCode())
                            s->warning("statement is not reachable");
                    }
                    else
                    {
                        result &= ~BEfallthru;
                        result |= blockExit(s, func, mustNotThrow);
                    }
                    slast = s;
                }
            }
        }

        void visit(UnrolledLoopStatement *uls)
        {
            result = BEfallthru;
            for (size_t i = 0; i < uls->statements->length; i++)
            {
                Statement *s = (*uls->statements)[i];
                if (s)
                {
                    int r = blockExit(s, func, mustNotThrow);
                    result |= r & ~(BEbreak | BEcontinue | BEfallthru);
                    if ((r & (BEfallthru | BEcontinue | BEbreak)) == 0)
                        result &= ~BEfallthru;
                }
            }
        }

        void visit(ScopeStatement *s)
        {
            //printf("ScopeStatement::blockExit(%p)\n", s->statement);
            result = s->statement ? blockExit(s->statement, func, mustNotThrow) : BEfallthru;
        }

        void visit(WhileStatement *)
        {
            assert(global.errors);
            result = BEfallthru;
        }

        void visit(DoStatement *s)
        {
            if (s->_body)
            {
                result = blockExit(s->_body, func, mustNotThrow);
                if (result == BEbreak)
                {
                    result = BEfallthru;
                    return;
                }
                if (result & BEcontinue)
                    result |= BEfallthru;
            }
            else
                result = BEfallthru;
            if (result & BEfallthru)
            {
                if (canThrow(s->condition, func, mustNotThrow))
                    result |= BEthrow;
                if (!(result & BEbreak) && s->condition->isBool(true))
                    result &= ~BEfallthru;
            }
            result &= ~(BEbreak | BEcontinue);
        }

        void visit(ForStatement *s)
        {
            result = BEfallthru;
            if (s->_init)
            {
                result = blockExit(s->_init, func, mustNotThrow);
                if (!(result & BEfallthru))
                    return;
            }
            if (s->condition)
            {
                if (canThrow(s->condition, func, mustNotThrow))
                    result |= BEthrow;
                if (s->condition->isBool(true))
                    result &= ~BEfallthru;
                else if (s->condition->isBool(false))
                    return;
            }
            else
                result &= ~BEfallthru;  // the body must do the exiting
            if (s->_body)
            {
                int r = blockExit(s->_body, func, mustNotThrow);
                if (r & (BEbreak | BEgoto))
                    result |= BEfallthru;
                result |= r & ~(BEfallthru | BEbreak | BEcontinue);
            }
            if (s->increment && canThrow(s->increment, func, mustNotThrow))
                result |= BEthrow;
        }

        void visit(ForeachStatement *s)
        {
            result = BEfallthru;
            if (canThrow(s->aggr, func, mustNotThrow))
                result |= BEthrow;
            if (s->_body)
                result |= blockExit(s->_body, func, mustNotThrow) & ~(BEbreak | BEcontinue);
        }

        void visit(ForeachRangeStatement *)
        {
            assert(global.errors);
            result = BEfallthru;
        }

        void visit(IfStatement *s)
        {
            //printf("IfStatement::blockExit(%p)\n", s);

            result = BEnone;
            if (canThrow(s->condition, func, mustNotThrow))
                result |= BEthrow;
            if (s->condition->isBool(true))
            {
                if (s->ifbody)
                    result |= blockExit(s->ifbody, func, mustNotThrow);
                else
                    result |= BEfallthru;
            }
            else if (s->condition->isBool(false))
            {
                if (s->elsebody)
                    result |= blockExit(s->elsebody, func, mustNotThrow);
                else
                    result |= BEfallthru;
            }
            else
            {
                if (s->ifbody)
                    result |= blockExit(s->ifbody, func, mustNotThrow);
                else
                    result |= BEfallthru;
                if (s->elsebody)
                    result |= blockExit(s->elsebody, func, mustNotThrow);
                else
                    result |= BEfallthru;
            }
            //printf("IfStatement::blockExit(%p) = x%x\n", s, result);
        }

        void visit(ConditionalStatement *s)
        {
            result = blockExit(s->ifbody, func, mustNotThrow);
            if (s->elsebody)
                result |= blockExit(s->elsebody, func, mustNotThrow);
        }

        void visit(PragmaStatement *)
        {
            result = BEfallthru;
        }

        void visit(StaticAssertStatement *)
        {
            result = BEfallthru;
        }

        void visit(SwitchStatement *s)
        {
            result = BEnone;
            if (canThrow(s->condition, func, mustNotThrow))
                result |= BEthrow;
            if (s->_body)
            {
                result |= blockExit(s->_body, func, mustNotThrow);
                if (result & BEbreak)
                {
                    result |= BEfallthru;
                    result &= ~BEbreak;
                }
            }
            else
                result |= BEfallthru;
        }

        void visit(CaseStatement *s)
        {
            result = blockExit(s->statement, func, mustNotThrow);
        }

        void visit(DefaultStatement *s)
        {
            result = blockExit(s->statement, func, mustNotThrow);
        }

        void visit(GotoDefaultStatement *)
        {
            result = BEgoto;
        }

        void visit(GotoCaseStatement *)
        {
            result = BEgoto;
        }

        void visit(SwitchErrorStatement *)
        {
            // Switch errors are non-recoverable
            result = BEhalt;
        }

        void visit(ReturnStatement *s)
        {
            result = BEreturn;
            if (s->exp && canThrow(s->exp, func, mustNotThrow))
                result |= BEthrow;
        }

        void visit(BreakStatement *s)
        {
            //printf("BreakStatement::blockExit(%p) = x%x\n", s, s->ident ? BEgoto : BEbreak);
            result = s->ident ? BEgoto : BEbreak;
        }

        void visit(ContinueStatement *s)
        {
            result = s->ident ? BEgoto : BEcontinue;
        }

        void visit(SynchronizedStatement *s)
        {
            result = s->_body ? blockExit(s->_body, func, mustNotThrow) : BEfallthru;
        }

        void visit(WithStatement *s)
        {
            result = BEnone;
            if (canThrow(s->exp, func, mustNotThrow))
                result = BEthrow;
            if (s->_body)
                result |= blockExit(s->_body, func, mustNotThrow);
            else
                result |= BEfallthru;
        }

        void visit(TryCatchStatement *s)
        {
            assert(s->_body);
            result = blockExit(s->_body, func, false);

            int catchresult = 0;
            for (size_t i = 0; i < s->catches->length; i++)
            {
                Catch *c = (*s->catches)[i];
                if (c->type == Type::terror)
                    continue;

                int cresult;
                if (c->handler)
                    cresult = blockExit(c->handler, func, mustNotThrow);
                else
                    cresult = BEfallthru;

                /* If we're catching Object, then there is no throwing
                 */
                Identifier *id = c->type->toBasetype()->isClassHandle()->ident;
                if (c->internalCatch && (cresult & BEfallthru))
                {
                    // Bugzilla 11542: leave blockExit flags of the body
                    cresult &= ~BEfallthru;
                }
                else if (id == Id::Object || id == Id::Throwable)
                {
                    result &= ~(BEthrow | BEerrthrow);
                }
                else if (id == Id::Exception)
                {
                    result &= ~BEthrow;
                }
                catchresult |= cresult;
            }
            if (mustNotThrow && (result & BEthrow))
            {
                // now explain why this is nothrow
                blockExit(s->_body, func, mustNotThrow);
            }
            result |= catchresult;
        }

        void visit(TryFinallyStatement *s)
        {
            result = BEfallthru;
            if (s->_body)
                result = blockExit(s->_body, func, false);

            // check finally body as well, it may throw (bug #4082)
            int finalresult = BEfallthru;
            if (s->finalbody)
                finalresult = blockExit(s->finalbody, func, false);

            // If either body or finalbody halts
            if (result == BEhalt)
                finalresult = BEnone;
            if (finalresult == BEhalt)
                result = BEnone;

            if (mustNotThrow)
            {
                // now explain why this is nothrow
                if (s->_body && (result & BEthrow))
                    blockExit(s->_body, func, mustNotThrow);
                if (s->finalbody && (finalresult & BEthrow))
                    blockExit(s->finalbody, func, mustNotThrow);
            }

        #if 0
            // Bugzilla 13201: Mask to prevent spurious warnings for
            // destructor call, exit of synchronized statement, etc.
            if (result == BEhalt && finalresult != BEhalt && s->finalbody &&
                s->finalbody->hasCode())
            {
                s->finalbody->warning("statement is not reachable");
            }
        #endif

            if (!(finalresult & BEfallthru))
                result &= ~BEfallthru;
            result |= finalresult & ~BEfallthru;
        }

        void visit(ScopeGuardStatement *)
        {
            // At this point, this statement is just an empty placeholder
            result = BEfallthru;
        }

        void visit(ThrowStatement *s)
        {
            if (s->internalThrow)
            {
                // Bugzilla 8675: Allow throwing 'Throwable' object even if mustNotThrow.
                result = BEfallthru;
                return;
            }

            Type *t = s->exp->type->toBasetype();
            ClassDeclaration *cd = t->isClassHandle();
            assert(cd);

            if (cd == ClassDeclaration::errorException ||
                ClassDeclaration::errorException->isBaseOf(cd, NULL))
            {
                result = BEerrthrow;
                return;
            }
            if (mustNotThrow)
                s->error("%s is thrown but not caught", s->exp->type->toChars());

            result = BEthrow;
        }

        void visit(GotoStatement *)
        {
            //printf("GotoStatement::blockExit(%p)\n", s);
            result = BEgoto;
        }

        void visit(LabelStatement *s)
        {
            //printf("LabelStatement::blockExit(%p)\n", s);
            result = s->statement ? blockExit(s->statement, func, mustNotThrow) : BEfallthru;
            if (s->breaks)
                result |= BEfallthru;
        }

        void visit(CompoundAsmStatement *s)
        {
            if (mustNotThrow && !(s->stc & STCnothrow))
                s->deprecation("asm statement is assumed to throw - mark it with `nothrow` if it does not");

            // Assume the worst
            result = BEfallthru | BEreturn | BEgoto | BEhalt;
            if (!(s->stc & STCnothrow)) result |= BEthrow;
        }

        void visit(ImportStatement *)
        {
            result = BEfallthru;
        }
    };

    if (!s)
        return BEfallthru;
    BlockExit be(func, mustNotThrow);
    s->accept(&be);
    return be.result;
}
