
/* Compiler implementation of the D programming language
 * Copyright (C) 1999-2024 by The D Language Foundation, All Rights Reserved
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
struct Scope;

namespace dmd
{
    bool genTypeInfo(Expression *e, const Loc &loc, Type *torig, Scope *sc);
    bool isSpeculativeType(Type *t);
    bool builtinTypeInfo(Type *t);
}
Type *getTypeInfoType(const Loc &loc, Type *t, Scope *sc);
