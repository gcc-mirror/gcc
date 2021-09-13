
/* Compiler implementation of the D programming language
 * Copyright (C) 1999-2021 by The D Language Foundation, All Rights Reserved
 * written by Walter Bright
 * http://www.digitalmars.com
 * Distributed under the Boost Software License, Version 1.0.
 * http://www.boost.org/LICENSE_1_0.txt
 * https://github.com/dlang/dmd/blob/master/src/version.h
 */

#pragma once

#include "dsymbol.h"

class DebugSymbol : public Dsymbol
{
public:
    unsigned level;

    DebugSymbol(Loc loc, Identifier *ident);
    DebugSymbol(Loc loc, unsigned level);
    Dsymbol *syntaxCopy(Dsymbol *);

    const char *toChars();
    void addMember(Scope *sc, ScopeDsymbol *sds);
    const char *kind() const;
    DebugSymbol *isDebugSymbol() { return this; }
    void accept(Visitor *v) { v->visit(this); }
};

class VersionSymbol : public Dsymbol
{
public:
    unsigned level;

    VersionSymbol(Loc loc, Identifier *ident);
    VersionSymbol(Loc loc, unsigned level);
    Dsymbol *syntaxCopy(Dsymbol *);

    const char *toChars();
    void addMember(Scope *sc, ScopeDsymbol *sds);
    const char *kind() const;
    VersionSymbol *isVersionSymbol() { return this; }
    void accept(Visitor *v) { v->visit(this); }
};
