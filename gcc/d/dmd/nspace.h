
/* Compiler implementation of the D programming language
 * Copyright (C) 1999-2022 by The D Language Foundation, All Rights Reserved
 * written by Walter Bright
 * https://www.digitalmars.com
 * Distributed under the Boost Software License, Version 1.0.
 * https://www.boost.org/LICENSE_1_0.txt
 * https://github.com/dlang/dmd/blob/master/src/dmd/nspace.h
 */

#pragma once

#include "dsymbol.h"

/* A namespace corresponding to a C++ namespace.
 * Implies extern(C++).
 */

class Nspace : public ScopeDsymbol
{
  public:
    Expression *identExp;
    Nspace *syntaxCopy(Dsymbol *s);
    void addMember(Scope *sc, ScopeDsymbol *sds);
    void setScope(Scope *sc);
    Dsymbol *search(const Loc &loc, Identifier *ident, int flags = SearchLocalsOnly);
    bool hasPointers();
    void setFieldOffset(AggregateDeclaration *ad, FieldState& fieldState, bool isunion);
    const char *kind() const;
    Nspace *isNspace() { return this; }
    void accept(Visitor *v) { v->visit(this); }
};
