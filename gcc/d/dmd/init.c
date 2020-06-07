
/* Compiler implementation of the D programming language
 * Copyright (C) 1999-2020 by The D Language Foundation, All Rights Reserved
 * written by Walter Bright
 * http://www.digitalmars.com
 * Distributed under the Boost Software License, Version 1.0.
 * http://www.boost.org/LICENSE_1_0.txt
 * https://github.com/D-Programming-Language/dmd/blob/master/src/init.c
 */

#include "root/dsystem.h"
#include "root/checkedint.h"

#include "mars.h"
#include "init.h"
#include "expression.h"
#include "statement.h"
#include "identifier.h"
#include "declaration.h"
#include "aggregate.h"
#include "scope.h"
#include "mtype.h"
#include "hdrgen.h"
#include "template.h"
#include "id.h"
#include "tokens.h"

Expression *semantic(Expression *e, Scope *sc);
Initializer *semantic(Initializer *init, Scope *sc, Type *t, NeedInterpret needInterpret);

/********************************** Initializer *******************************/

Initializer::Initializer(Loc loc)
{
    this->loc = loc;
}

Initializers *Initializer::arraySyntaxCopy(Initializers *ai)
{
    Initializers *a = NULL;
    if (ai)
    {
        a = new Initializers();
        a->setDim(ai->length);
        for (size_t i = 0; i < a->length; i++)
            (*a)[i] = (*ai)[i]->syntaxCopy();
    }
    return a;
}

const char *Initializer::toChars()
{
    OutBuffer buf;
    HdrGenState hgs;
    ::toCBuffer(this, &buf, &hgs);
    return buf.extractChars();
}

/********************************** ErrorInitializer ***************************/

ErrorInitializer::ErrorInitializer()
    : Initializer(Loc())
{
}

Initializer *ErrorInitializer::syntaxCopy()
{
    return this;
}

/********************************** VoidInitializer ***************************/

VoidInitializer::VoidInitializer(Loc loc)
    : Initializer(loc)
{
    type = NULL;
}

Initializer *VoidInitializer::syntaxCopy()
{
    return new VoidInitializer(loc);
}

/********************************** StructInitializer *************************/

StructInitializer::StructInitializer(Loc loc)
    : Initializer(loc)
{
}

Initializer *StructInitializer::syntaxCopy()
{
    StructInitializer *ai = new StructInitializer(loc);
    assert(field.length == value.length);
    ai->field.setDim(field.length);
    ai->value.setDim(value.length);
    for (size_t i = 0; i < field.length; i++)
    {
        ai->field[i] = field[i];
        ai->value[i] = value[i]->syntaxCopy();
    }
    return ai;
}

void StructInitializer::addInit(Identifier *field, Initializer *value)
{
    //printf("StructInitializer::addInit(field = %p, value = %p)\n", field, value);
    this->field.push(field);
    this->value.push(value);
}

/********************************** ArrayInitializer ************************************/

ArrayInitializer::ArrayInitializer(Loc loc)
    : Initializer(loc)
{
    dim = 0;
    type = NULL;
    sem = false;
}

Initializer *ArrayInitializer::syntaxCopy()
{
    //printf("ArrayInitializer::syntaxCopy()\n");
    ArrayInitializer *ai = new ArrayInitializer(loc);
    assert(index.length == value.length);
    ai->index.setDim(index.length);
    ai->value.setDim(value.length);
    for (size_t i = 0; i < ai->value.length; i++)
    {
        ai->index[i] = index[i] ? index[i]->syntaxCopy() : NULL;
        ai->value[i] = value[i]->syntaxCopy();
    }
    return ai;
}

void ArrayInitializer::addInit(Expression *index, Initializer *value)
{
    this->index.push(index);
    this->value.push(value);
    dim = 0;
    type = NULL;
}

bool ArrayInitializer::isAssociativeArray()
{
    for (size_t i = 0; i < value.length; i++)
    {
        if (index[i])
            return true;
    }
    return false;
}

/********************************
 * If possible, convert array initializer to associative array initializer.
 */

Expression *ArrayInitializer::toAssocArrayLiteral()
{
    Expression *e;

    //printf("ArrayInitializer::toAssocArrayInitializer()\n");
    //static int i; if (++i == 2) halt();
    Expressions *keys = new Expressions();
    keys->setDim(value.length);
    Expressions *values = new Expressions();
    values->setDim(value.length);

    for (size_t i = 0; i < value.length; i++)
    {
        e = index[i];
        if (!e)
            goto Lno;
        (*keys)[i] = e;

        Initializer *iz = value[i];
        if (!iz)
            goto Lno;
        e = initializerToExpression(iz);
        if (!e)
            goto Lno;
        (*values)[i] = e;
    }
    e = new AssocArrayLiteralExp(loc, keys, values);
    return e;

Lno:
    delete keys;
    delete values;
    error(loc, "not an associative array initializer");
    return new ErrorExp();
}

/********************************** ExpInitializer ************************************/

ExpInitializer::ExpInitializer(Loc loc, Expression *exp)
    : Initializer(loc)
{
    this->exp = exp;
    this->expandTuples = false;
}

Initializer *ExpInitializer::syntaxCopy()
{
    return new ExpInitializer(loc, exp->syntaxCopy());
}

#if 1   // should be removed and rely on ctfeInterpreter()
bool arrayHasNonConstPointers(Expressions *elems);

bool hasNonConstPointers(Expression *e)
{
    if (e->type->ty == Terror)
        return false;

    if (e->op == TOKnull)
        return false;
    if (e->op == TOKstructliteral)
    {
        StructLiteralExp *se = (StructLiteralExp *)e;
        return arrayHasNonConstPointers(se->elements);
    }
    if (e->op == TOKarrayliteral)
    {
        if (!e->type->nextOf()->hasPointers())
            return false;
        ArrayLiteralExp *ae = (ArrayLiteralExp *)e;
        return arrayHasNonConstPointers(ae->elements);
    }
    if (e->op == TOKassocarrayliteral)
    {
        AssocArrayLiteralExp *ae = (AssocArrayLiteralExp *)e;
        if (ae->type->nextOf()->hasPointers() &&
            arrayHasNonConstPointers(ae->values))
                return true;
        if (((TypeAArray *)ae->type)->index->hasPointers())
            return arrayHasNonConstPointers(ae->keys);
        return false;
    }
    if (e->op == TOKaddress)
    {
        AddrExp *ae = (AddrExp *)e;
        if (ae->e1->op == TOKstructliteral)
        {
            StructLiteralExp *se = (StructLiteralExp *)ae->e1;
            if (!(se->stageflags & stageSearchPointers))
            {
                int old = se->stageflags;
                se->stageflags |= stageSearchPointers;
                bool ret = arrayHasNonConstPointers(se->elements);
                se->stageflags = old;
                return ret;
            }
            else
            {
                return false;
            }
        }
        return true;
    }
    if (e->type->ty== Tpointer && e->type->nextOf()->ty != Tfunction)
    {
        if (e->op == TOKsymoff) // address of a global is OK
            return false;
        if (e->op == TOKint64)  // cast(void *)int is OK
            return false;
        if (e->op == TOKstring) // "abc".ptr is OK
            return false;
        return true;
    }
    return false;
}

bool arrayHasNonConstPointers(Expressions *elems)
{
    for (size_t i = 0; i < elems->length; i++)
    {
        Expression *e = (*elems)[i];
        if (e && hasNonConstPointers(e))
            return true;
    }
    return false;
}
#endif
