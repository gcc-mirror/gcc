
/* Compiler implementation of the D programming language
 * Copyright (C) 2018-2020 by The D Language Foundation, All Rights Reserved
 * written by Walter Bright
 * http://www.digitalmars.com
 * Distributed under the Boost Software License, Version 1.0.
 * http://www.boost.org/LICENSE_1_0.txt
 * https://github.com/D-Programming-Language/dmd/blob/master/src/iasm.c
 */

/* Inline assembler for the D programming language compiler
 */

#include "scope.h"
#include "declaration.h"
#include "statement.h"

#ifdef IN_GCC
Statement *gccAsmSemantic(GccAsmStatement *s, Scope *sc);
#else
Statement *inlineAsmSemantic(InlineAsmStatement *s, Scope *sc);
#endif

Statement *asmSemantic(AsmStatement *s, Scope *sc)
{
    //printf("AsmStatement::semantic()\n");

    FuncDeclaration *fd = sc->parent->isFuncDeclaration();
    assert(fd);

    if (!s->tokens)
        return NULL;

    // Assume assembler code takes care of setting the return value
    sc->func->hasReturnExp |= 8;

#ifdef IN_GCC
    GccAsmStatement *eas = new GccAsmStatement(s->loc, s->tokens);
    return gccAsmSemantic(eas, sc);
#else
    InlineAsmStatement *ias = new InlineAsmStatement(s->loc, s->tokens);
    return inlineAsmSemantic(ias, sc);
#endif
}
