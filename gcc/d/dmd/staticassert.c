
/* Compiler implementation of the D programming language
 * Copyright (C) 1999-2019 by The D Language Foundation, All Rights Reserved
 * written by Walter Bright
 * http://www.digitalmars.com
 * Distributed under the Boost Software License, Version 1.0.
 * http://www.boost.org/LICENSE_1_0.txt
 * https://github.com/D-Programming-Language/dmd/blob/master/src/staticassert.c
 */

#include "root/dsystem.h"

#include "mars.h"
#include "dsymbol.h"
#include "staticassert.h"
#include "expression.h"
#include "id.h"
#include "scope.h"
#include "template.h"
#include "declaration.h"

Expression *semantic(Expression *e, Scope *sc);
bool evalStaticCondition(Scope *sc, Expression *exp, Expression *e, bool &errors);

/********************************* AttribDeclaration ****************************/

StaticAssert::StaticAssert(Loc loc, Expression *exp, Expression *msg)
        : Dsymbol(Id::empty)
{
    this->loc = loc;
    this->exp = exp;
    this->msg = msg;
}

Dsymbol *StaticAssert::syntaxCopy(Dsymbol *s)
{
    assert(!s);
    return new StaticAssert(loc, exp->syntaxCopy(), msg ? msg->syntaxCopy() : NULL);
}

void StaticAssert::addMember(Scope *, ScopeDsymbol *)
{
    // we didn't add anything
}

void StaticAssert::semantic(Scope *)
{
}

void StaticAssert::semantic2(Scope *sc)
{
    //printf("StaticAssert::semantic2() %s\n", toChars());
    ScopeDsymbol *sds = new ScopeDsymbol();
    sc = sc->push(sds);
    sc->tinst = NULL;
    sc->minst = NULL;

    bool errors = false;
    bool result = evalStaticCondition(sc, exp, exp, errors);
    sc = sc->pop();
    if (errors)
    {
        errorSupplemental(loc, "while evaluating: static assert(%s)", exp->toChars());
    }
    else if (!result)
    {
        if (msg)
        {
            sc = sc->startCTFE();
            msg = ::semantic(msg, sc);
            msg = resolveProperties(sc, msg);
            sc = sc->endCTFE();
            msg = msg->ctfeInterpret();
            if (StringExp * se = msg->toStringExp())
            {
                // same with pragma(msg)
                se = se->toUTF8(sc);
                error("\"%.*s\"", (int)se->len, (char *)se->string);
            }
            else
                error("%s", msg->toChars());
        }
        else
            error("(%s) is false", exp->toChars());
        if (sc->tinst)
            sc->tinst->printInstantiationTrace();
        if (!global.gag)
              fatal();
    }
}

bool StaticAssert::oneMember(Dsymbol **ps, Identifier *)
{
    //printf("StaticAssert::oneMember())\n");
    *ps = NULL;
    return true;
}

const char *StaticAssert::kind() const
{
    return "static assert";
}
