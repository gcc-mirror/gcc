
/* Compiler implementation of the D programming language
 * Copyright (C) 2009-2019 by The D Language Foundation, All Rights Reserved
 * written by Walter Bright
 * http://www.digitalmars.com
 * Distributed under the Boost Software License, Version 1.0.
 * http://www.boost.org/LICENSE_1_0.txt
 * https://github.com/D-Programming-Language/dmd/blob/master/src/aliasthis.c
 */

#include "root/dsystem.h"

#include "mars.h"
#include "identifier.h"
#include "aliasthis.h"
#include "scope.h"
#include "aggregate.h"
#include "dsymbol.h"
#include "mtype.h"
#include "declaration.h"
#include "tokens.h"

Expression *semantic(Expression *e, Scope *sc);

Expression *resolveAliasThis(Scope *sc, Expression *e, bool gag)
{
    AggregateDeclaration *ad = isAggregate(e->type);

    if (ad && ad->aliasthis)
    {
        unsigned olderrors = gag ? global.startGagging() : 0;

        Loc loc = e->loc;
        Type *tthis = (e->op == TOKtype ? e->type : NULL);
        e = new DotIdExp(loc, e, ad->aliasthis->ident);
        e = semantic(e, sc);
        if (tthis && ad->aliasthis->needThis())
        {
            if (e->op == TOKvar)
            {
                if (FuncDeclaration *fd = ((VarExp *)e)->var->isFuncDeclaration())
                {
                    // Bugzilla 13009: Support better match for the overloaded alias this.
                    bool hasOverloads = false;
                    if (FuncDeclaration *f = fd->overloadModMatch(loc, tthis, hasOverloads))
                    {
                        if (!hasOverloads)
                            fd = f;     // use exact match
                        e = new VarExp(loc, fd, hasOverloads);
                        e->type = f->type;
                        e = new CallExp(loc, e);
                        goto L1;
                    }
                }
            }
            /* non-@property function is not called inside typeof(),
             * so resolve it ahead.
             */
            {
            int save = sc->intypeof;
            sc->intypeof = 1;   // bypass "need this" error check
            e = resolveProperties(sc, e);
            sc->intypeof = save;
            }

        L1:
            e = new TypeExp(loc, new TypeTypeof(loc, e));
            e = semantic(e, sc);
        }
        e = resolveProperties(sc, e);

        if (gag && global.endGagging(olderrors))
            e = NULL;
    }

    return e;
}

AliasThis::AliasThis(Loc loc, Identifier *ident)
    : Dsymbol(NULL)             // it's anonymous (no identifier)
{
    this->loc = loc;
    this->ident = ident;
}

Dsymbol *AliasThis::syntaxCopy(Dsymbol *s)
{
    assert(!s);
    return new AliasThis(loc, ident);
}

void AliasThis::semantic(Scope *sc)
{
    if (semanticRun != PASSinit)
        return;

    if (_scope)
    {
        sc = _scope;
        _scope = NULL;
    }

    if (!sc)
        return;

    semanticRun = PASSsemantic;

    Dsymbol *p = sc->parent->pastMixin();
    AggregateDeclaration *ad = p->isAggregateDeclaration();
    if (!ad)
    {
        ::error(loc, "alias this can only be a member of aggregate, not %s %s",
            p->kind(), p->toChars());
        return;
    }

    assert(ad->members);
    Dsymbol *s = ad->search(loc, ident);
    if (!s)
    {
        s = sc->search(loc, ident, NULL);
        if (s)
            ::error(loc, "%s is not a member of %s", s->toChars(), ad->toChars());
        else
            ::error(loc, "undefined identifier %s", ident->toChars());
        return;
    }
    else if (ad->aliasthis && s != ad->aliasthis)
    {
        ::error(loc, "there can be only one alias this");
        return;
    }

    if (ad->type->ty == Tstruct && ((TypeStruct *)ad->type)->sym != ad)
    {
        AggregateDeclaration *ad2 = ((TypeStruct *)ad->type)->sym;
        assert(ad2->type == Type::terror);
        ad->aliasthis = ad2->aliasthis;
        return;
    }

    /* disable the alias this conversion so the implicit conversion check
     * doesn't use it.
     */
    ad->aliasthis = NULL;

    Dsymbol *sx = s;
    if (sx->isAliasDeclaration())
        sx = sx->toAlias();
    Declaration *d = sx->isDeclaration();
    if (d && !d->isTupleDeclaration())
    {
        Type *t = d->type;
        assert(t);
        if (ad->type->implicitConvTo(t) > MATCHnomatch)
        {
            ::error(loc, "alias this is not reachable as %s already converts to %s", ad->toChars(), t->toChars());
        }
    }

    ad->aliasthis = s;
    semanticRun = PASSsemanticdone;
}

const char *AliasThis::kind() const
{
    return "alias this";
}
