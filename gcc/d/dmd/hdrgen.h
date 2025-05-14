
/* Compiler implementation of the D programming language
 * Copyright (C) 1999-2025 by The D Language Foundation, All Rights Reserved
 * written by Dave Fladebo
 * https://www.digitalmars.com
 * Distributed under the Boost Software License, Version 1.0.
 * https://www.boost.org/LICENSE_1_0.txt
 * https://github.com/dlang/dmd/blob/master/src/dmd/hdrgen.h
 */

#pragma once

#include "globals.h"
#include "mtype.h"

class Expression;
class Initializer;
class Module;
class Statement;

namespace dmd
{
    void genhdrfile(Module *m, bool doFuncBodies, OutBuffer &buf);
    void genCppHdrFiles(Modules &ms);
    void moduleToBuffer(OutBuffer& buf, bool vcg_ast, Module *m);
    const char *parametersTypeToChars(ParameterList pl);

    const char* toChars(const Expression* const e);
    const char* toChars(const Initializer* const i);
    const char* toChars(const Statement* const s);
    const char* toChars(const Type* const t);
}
