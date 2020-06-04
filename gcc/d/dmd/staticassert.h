
/* Compiler implementation of the D programming language
 * Copyright (C) 1999-2020 by The D Language Foundation, All Rights Reserved
 * written by Walter Bright
 * http://www.digitalmars.com
 * Distributed under the Boost Software License, Version 1.0.
 * http://www.boost.org/LICENSE_1_0.txt
 * https://github.com/dlang/dmd/blob/master/src/staticassert.h
 */

#pragma once

#include "dsymbol.h"

class Expression;

class StaticAssert : public Dsymbol
{
public:
    Expression *exp;
    Expression *msg;

    StaticAssert(Loc loc, Expression *exp, Expression *msg);

    Dsymbol *syntaxCopy(Dsymbol *s);
    void addMember(Scope *sc, ScopeDsymbol *sds);
    void semantic(Scope *sc);
    void semantic2(Scope *sc);
    bool oneMember(Dsymbol **ps, Identifier *ident);
    const char *kind() const;
    void accept(Visitor *v) { v->visit(this); }
};
