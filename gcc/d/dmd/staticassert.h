
/* Compiler implementation of the D programming language
 * Copyright (C) 1999-2023 by The D Language Foundation, All Rights Reserved
 * written by Walter Bright
 * https://www.digitalmars.com
 * Distributed under the Boost Software License, Version 1.0.
 * https://www.boost.org/LICENSE_1_0.txt
 * https://github.com/dlang/dmd/blob/master/src/dmd/staticassert.h
 */

#pragma once

#include "dsymbol.h"

class Expression;

class StaticAssert : public Dsymbol
{
public:
    Expression *exp;
    Expressions *msg;

    StaticAssert *syntaxCopy(Dsymbol *s) override;
    void addMember(Scope *sc, ScopeDsymbol *sds) override;
    bool oneMember(Dsymbol **ps, Identifier *ident) override;
    const char *kind() const override;
    StaticAssert *isStaticAssert() override { return this; }
    void accept(Visitor *v) override { v->visit(this); }
};
