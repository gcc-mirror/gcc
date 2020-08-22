
/* Compiler implementation of the D programming language
 * Copyright (C) 1999-2020 by The D Language Foundation, All Rights Reserved
 * written by Walter Bright
 * http://www.digitalmars.com
 * Distributed under the Boost Software License, Version 1.0.
 * http://www.boost.org/LICENSE_1_0.txt
 * https://github.com/D-Programming-Language/dmd/blob/master/src/canthrow.c
 */

#include "root/dsystem.h"

#include "mars.h"
#include "init.h"
#include "expression.h"
#include "template.h"
#include "statement.h"
#include "mtype.h"
#include "utf.h"
#include "declaration.h"
#include "aggregate.h"
#include "scope.h"
#include "attrib.h"
#include "tokens.h"

bool Dsymbol_canThrow(Dsymbol *s, FuncDeclaration *func, bool mustNotThrow);
bool walkPostorder(Expression *e, StoppableVisitor *v);

/********************************************
 * Returns true if the expression may throw exceptions.
 * If 'mustNotThrow' is true, generate an error if it throws
 */

bool canThrow(Expression *e, FuncDeclaration *func, bool mustNotThrow)
{
    //printf("Expression::canThrow(%d) %s\n", mustNotThrow, toChars());

    // stop walking if we determine this expression can throw
    class CanThrow : public StoppableVisitor
    {
        FuncDeclaration *func;
        bool mustNotThrow;

    public:
        CanThrow(FuncDeclaration *func, bool mustNotThrow)
            : func(func), mustNotThrow(mustNotThrow)
        {
        }

        void visit(Expression *)
        {
        }

        void visit(DeclarationExp *de)
        {
            stop = Dsymbol_canThrow(de->declaration, func, mustNotThrow);
        }

        void visit(CallExp *ce)
        {
            if (global.errors && !ce->e1->type)
                return;                       // error recovery

            /* If calling a function or delegate that is typed as nothrow,
             * then this expression cannot throw.
             * Note that pure functions can throw.
             */
            Type *t = ce->e1->type->toBasetype();
            if (ce->f && ce->f == func)
                return;
            if (t->ty == Tfunction && ((TypeFunction *)t)->isnothrow)
                return;
            if (t->ty == Tdelegate && ((TypeFunction *)((TypeDelegate *)t)->next)->isnothrow)
                return;

            if (mustNotThrow)
            {
                if (ce->f)
                {
                    ce->error("%s '%s' is not nothrow",
                        ce->f->kind(), ce->f->toPrettyChars());
                }
                else
                {
                    Expression *e1 = ce->e1;
                    if (e1->op == TOKstar)   // print 'fp' if e1 is (*fp)
                        e1 = ((PtrExp *)e1)->e1;
                    ce->error("'%s' is not nothrow", e1->toChars());
                }
            }
            stop = true;
        }

        void visit(NewExp *ne)
        {
            if (ne->member)
            {
                if (ne->allocator)
                {
                    // Bugzilla 14407
                    Type *t = ne->allocator->type->toBasetype();
                    if (t->ty == Tfunction && !((TypeFunction *)t)->isnothrow)
                    {
                        if (mustNotThrow)
                        {
                            ne->error("%s '%s' is not nothrow",
                                ne->allocator->kind(), ne->allocator->toPrettyChars());
                        }
                        stop = true;
                    }
                }
                // See if constructor call can throw
                Type *t = ne->member->type->toBasetype();
                if (t->ty == Tfunction && !((TypeFunction *)t)->isnothrow)
                {
                    if (mustNotThrow)
                    {
                        ne->error("%s '%s' is not nothrow",
                            ne->member->kind(), ne->member->toPrettyChars());
                    }
                    stop = true;
                }
            }
            // regard storage allocation failures as not recoverable
        }

        void visit(DeleteExp *de)
        {
            Type *tb = de->e1->type->toBasetype();
            AggregateDeclaration *ad = NULL;
            switch (tb->ty)
            {
            case Tclass:
                ad = ((TypeClass *)tb)->sym;
                break;

            case Tpointer:
                tb = ((TypePointer *)tb)->next->toBasetype();
                if (tb->ty == Tstruct)
                    ad = ((TypeStruct *)tb)->sym;
                break;

            case Tarray:
            {
                Type *tv = tb->nextOf()->baseElemOf();
                if (tv->ty == Tstruct)
                {
                    ad = ((TypeStruct *)tv)->sym;
                    break;
                }
            }

            default:
                break;
            }
            if (!ad)
                return;

            if (ad->dtor)
            {
                Type *t = ad->dtor->type->toBasetype();
                if (t->ty == Tfunction && !((TypeFunction *)t)->isnothrow)
                {
                    if (mustNotThrow)
                    {
                        de->error("%s '%s' is not nothrow",
                            ad->dtor->kind(), ad->dtor->toPrettyChars());
                    }
                    stop = true;
                }
            }
            if (ad->aggDelete && tb->ty != Tarray)
            {
                Type *t = ad->aggDelete->type;
                if (t->ty == Tfunction && !((TypeFunction *)t)->isnothrow)
                {
                    if (mustNotThrow)
                    {
                        de->error("%s '%s' is not nothrow",
                            ad->aggDelete->kind(), ad->aggDelete->toPrettyChars());
                    }
                    stop = true;
                }
            }
        }

        void visit(AssignExp *ae)
        {
            // blit-init cannot throw
            if (ae->op == TOKblit)
                return;

            /* Element-wise assignment could invoke postblits.
             */
            Type *t;
            if (ae->type->toBasetype()->ty == Tsarray)
            {
                if (!ae->e2->isLvalue())
                    return;
                t = ae->type;
            }
            else if (ae->e1->op == TOKslice)
                t = ((SliceExp *)ae->e1)->e1->type;
            else
                return;

            Type *tv = t->baseElemOf();
            if (tv->ty != Tstruct)
                return;
            StructDeclaration *sd = ((TypeStruct *)tv)->sym;
            if (!sd->postblit || sd->postblit->type->ty != Tfunction)
                return;

            if (((TypeFunction *)sd->postblit->type)->isnothrow)
                ;
            else
            {
                if (mustNotThrow)
                {
                    ae->error("%s '%s' is not nothrow",
                        sd->postblit->kind(), sd->postblit->toPrettyChars());
                }
                stop = true;
            }
        }

        void visit(NewAnonClassExp *)
        {
            assert(0);          // should have been lowered by semantic()
        }
    };

    CanThrow ct(func, mustNotThrow);
    return walkPostorder(e, &ct);
}

/**************************************
 * Does symbol, when initialized, throw?
 * Mirrors logic in Dsymbol_toElem().
 */

bool Dsymbol_canThrow(Dsymbol *s, FuncDeclaration *func, bool mustNotThrow)
{
    AttribDeclaration *ad;
    VarDeclaration *vd;
    TemplateMixin *tm;
    TupleDeclaration *td;

    //printf("Dsymbol_toElem() %s\n", s->toChars());
    ad = s->isAttribDeclaration();
    if (ad)
    {
        Dsymbols *decl = ad->include(NULL);
        if (decl && decl->length)
        {
            for (size_t i = 0; i < decl->length; i++)
            {
                s = (*decl)[i];
                if (Dsymbol_canThrow(s, func, mustNotThrow))
                    return true;
            }
        }
    }
    else if ((vd = s->isVarDeclaration()) != NULL)
    {
        s = s->toAlias();
        if (s != vd)
            return Dsymbol_canThrow(s, func, mustNotThrow);
        if (vd->storage_class & STCmanifest)
            ;
        else if (vd->isStatic() || vd->storage_class & (STCextern | STCtls | STCgshared))
            ;
        else
        {
            if (vd->_init)
            {
                ExpInitializer *ie = vd->_init->isExpInitializer();
                if (ie && canThrow(ie->exp, func, mustNotThrow))
                    return true;
            }
            if (vd->needsScopeDtor())
                return canThrow(vd->edtor, func, mustNotThrow);
        }
    }
    else if ((tm = s->isTemplateMixin()) != NULL)
    {
        //printf("%s\n", tm->toChars());
        if (tm->members)
        {
            for (size_t i = 0; i < tm->members->length; i++)
            {
                Dsymbol *sm = (*tm->members)[i];
                if (Dsymbol_canThrow(sm, func, mustNotThrow))
                    return true;
            }
        }
    }
    else if ((td = s->isTupleDeclaration()) != NULL)
    {
        for (size_t i = 0; i < td->objects->length; i++)
        {
            RootObject *o = (*td->objects)[i];
            if (o->dyncast() == DYNCAST_EXPRESSION)
            {
                Expression *eo = (Expression *)o;
                if (eo->op == TOKdsymbol)
                {
                    DsymbolExp *se = (DsymbolExp *)eo;
                    if (Dsymbol_canThrow(se->s, func, mustNotThrow))
                        return true;
                }
            }
        }
    }
    return false;
}
