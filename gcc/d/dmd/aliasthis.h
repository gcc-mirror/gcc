
/* Compiler implementation of the D programming language
 * Copyright (C) 2009-2025 by The D Language Foundation, All Rights Reserved
 * written by Walter Bright
 * https://www.digitalmars.com
 * Distributed under the Boost Software License, Version 1.0.
 * https://www.boost.org/LICENSE_1_0.txt
 * https://github.com/dlang/dmd/blob/master/src/dmd/aliasthis.h
 */

#pragma once

#include "globals.h"
#include "dsymbol.h"

/**************************************************************/

class AliasThis final : public Dsymbol
{
public:
   // alias Identifier this;
    Identifier *ident;
    Dsymbol    *sym;
    d_bool     isDeprecated_;

    AliasThis *syntaxCopy(Dsymbol *) override;
    const char *kind() const override;
    AliasThis *isAliasThis() { return this; }
    void accept(Visitor *v) override { v->visit(this); }
    bool isDeprecated() const override { return this->isDeprecated_; }
};
