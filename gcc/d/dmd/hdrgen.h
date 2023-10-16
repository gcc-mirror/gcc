
/* Compiler implementation of the D programming language
 * Copyright (C) 1999-2023 by The D Language Foundation, All Rights Reserved
 * written by Dave Fladebo
 * https://www.digitalmars.com
 * Distributed under the Boost Software License, Version 1.0.
 * https://www.boost.org/LICENSE_1_0.txt
 * https://github.com/dlang/dmd/blob/master/src/dmd/hdrgen.h
 */

#pragma once

#include "globals.h"
#include "mtype.h"

class Module;

void genhdrfile(Module *m, OutBuffer &buf);
void genCppHdrFiles(Modules &ms);
void moduleToBuffer(OutBuffer& buf, Module *m);
const char *parametersTypeToChars(ParameterList pl);
