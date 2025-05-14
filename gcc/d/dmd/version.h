
/* Compiler implementation of the D programming language
 * Copyright (C) 1999-2025 by The D Language Foundation, All Rights Reserved
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
    DebugSymbol *syntaxCopy(Dsymbol *) override;

    const char *kind() const override;
    void accept(Visitor *v) override { v->visit(this); }
};

class VersionSymbol final : public Dsymbol
{
public:
    VersionSymbol *syntaxCopy(Dsymbol *) override;

    const char *kind() const override;
    void accept(Visitor *v) override { v->visit(this); }
};
