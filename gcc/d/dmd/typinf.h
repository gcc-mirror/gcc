
/* Compiler implementation of the D programming language
 * Copyright (C) 1999-2025 by The D Language Foundation, All Rights Reserved
 * written by Walter Bright
 * https://www.digitalmars.com
 * Distributed under the Boost Software License, Version 1.0.
 * https://www.boost.org/LICENSE_1_0.txt
 * https://github.com/dlang/dmd/blob/master/src/dmd/typinf.h
 */

#pragma once

#include "globals.h"

class Expression;
class Type;
class TypeAArray;
class TypeInfoDeclaration;
struct Scope;

namespace dmd
{
    bool genTypeInfo(Expression *e, Loc loc, Type *torig, Scope *sc);
    bool isSpeculativeType(Type *t);
    bool builtinTypeInfo(Type *t);
    Type *makeNakedAssociativeArray(TypeAArray *t);
    TypeInfoDeclaration *getTypeInfoAssocArrayDeclaration(TypeAArray *t, Scope *sc);
}
Type *getTypeInfoType(Loc loc, Type *t, Scope *sc);
