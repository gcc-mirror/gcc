
/* Compiler implementation of the D programming language
 * Copyright (C) 1999-2021 by The D Language Foundation, All Rights Reserved
 * written by Walter Bright
 * http://www.digitalmars.com
 * Distributed under the Boost Software License, Version 1.0.
 * http://www.boost.org/LICENSE_1_0.txt
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
    bool mangleOnly;
    Nspace(Loc loc, Identifier *ident, Dsymbols *members, bool mangleOnly);

    Dsymbol *syntaxCopy(Dsymbol *s);
    void addMember(Scope *sc, ScopeDsymbol *sds);
    void setScope(Scope *sc);
    bool oneMember(Dsymbol **ps, Identifier *ident);
    Dsymbol *search(const Loc &loc, Identifier *ident, int flags = SearchLocalsOnly);
    int apply(Dsymbol_apply_ft_t fp, void *param);
    bool hasPointers();
    void setFieldOffset(AggregateDeclaration *ad, unsigned *poffset, bool isunion);
    const char *kind() const;
    Nspace *isNspace() { return this; }
    void accept(Visitor *v) { v->visit(this); }
};
