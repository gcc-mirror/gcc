
/* Compiler implementation of the D programming language
 * Copyright (C) 1999-2023 by The D Language Foundation, All Rights Reserved
 * written by Walter Bright
 * https://www.digitalmars.com
 * Distributed under the Boost Software License, Version 1.0.
 * https://www.boost.org/LICENSE_1_0.txt
 * https://github.com/dlang/dmd/blob/master/src/dmd/version.h
 */

#pragma once

#include "dsymbol.h"

class DebugSymbol final : public Dsymbol
{
public:
    unsigned level;

    DebugSymbol *syntaxCopy(Dsymbol *) override;

    const char *toChars() const override;
    void addMember(Scope *sc, ScopeDsymbol *sds) override;
    const char *kind() const override;
    DebugSymbol *isDebugSymbol() override;
    void accept(Visitor *v) override { v->visit(this); }
};

class VersionSymbol final : public Dsymbol
{
public:
    unsigned level;

    VersionSymbol *syntaxCopy(Dsymbol *) override;

    const char *toChars() const override;
    void addMember(Scope *sc, ScopeDsymbol *sds) override;
    const char *kind() const override;
    VersionSymbol *isVersionSymbol() override;
    void accept(Visitor *v) override { v->visit(this); }
};
