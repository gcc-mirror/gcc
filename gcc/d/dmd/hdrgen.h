
/* Compiler implementation of the D programming language
 * Copyright (C) 1999-2021 by The D Language Foundation, All Rights Reserved
 * written by Dave Fladebo
 * http://www.digitalmars.com
 * Distributed under the Boost Software License, Version 1.0.
 * http://www.boost.org/LICENSE_1_0.txt
 * https://github.com/dlang/dmd/blob/master/src/dmd/hdrgen.h
 */

#pragma once

#include "root/dsystem.h"               // memset()
#include "dsymbol.h"

void genhdrfile(Module *m);

struct HdrGenState
{
    bool hdrgen;        // true if generating header file
    bool ddoc;          // true if generating Ddoc file
    bool fullDump;      // true if generating a full AST dump file
    bool fullQual;      // fully qualify types when printing
    int tpltMember;
    int autoMember;
    int forStmtInit;

    HdrGenState() { memset(this, 0, sizeof(HdrGenState)); }
};

void toCBuffer(Statement *s, OutBuffer *buf, HdrGenState *hgs);
void toCBuffer(Type *t, OutBuffer *buf, Identifier *ident, HdrGenState *hgs);
void toCBuffer(Dsymbol *s, OutBuffer *buf, HdrGenState *hgs);
void toCBuffer(Initializer *iz, OutBuffer *buf, HdrGenState *hgs);
void toCBuffer(Expression *e, OutBuffer *buf, HdrGenState *hgs);
void toCBuffer(TemplateParameter *tp, OutBuffer *buf, HdrGenState *hgs);

void toCBufferInstance(TemplateInstance *ti, OutBuffer *buf, bool qualifyTypes = false);

void functionToBufferFull(TypeFunction *tf, OutBuffer *buf, Identifier *ident, HdrGenState* hgs, TemplateDeclaration *td);
void functionToBufferWithIdent(TypeFunction *t, OutBuffer *buf, const char *ident);

void argExpTypesToCBuffer(OutBuffer *buf, Expressions *arguments);

void arrayObjectsToBuffer(OutBuffer *buf, Objects *objects);

void moduleToBuffer(OutBuffer *buf, Module *m);

const char *parametersTypeToChars(ParameterList pl);
const char *parameterToChars(Parameter *parameter, TypeFunction *tf, bool fullQual);

bool stcToBuffer(OutBuffer *buf, StorageClass stc);
const char *stcToChars(StorageClass& stc);
const char *linkageToChars(LINK linkage);
