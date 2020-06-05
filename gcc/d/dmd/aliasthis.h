
/* Compiler implementation of the D programming language
 * Copyright (C) 2009-2020 by The D Language Foundation, All Rights Reserved
 * written by Walter Bright
 * http://www.digitalmars.com
 * Distributed under the Boost Software License, Version 1.0.
 * http://www.boost.org/LICENSE_1_0.txt
 * https://github.com/dlang/dmd/blob/master/src/aliasthis.h
 */

#pragma once

#include "dsymbol.h"

/**************************************************************/

class AliasThis : public Dsymbol
{
public:
   // alias Identifier this;
    Identifier *ident;

    AliasThis(Loc loc, Identifier *ident);

    Dsymbol *syntaxCopy(Dsymbol *);
    void semantic(Scope *sc);
    const char *kind() const;
    AliasThis *isAliasThis() { return this; }
    void accept(Visitor *v) { v->visit(this); }
};
