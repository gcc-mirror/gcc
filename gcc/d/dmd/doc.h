
/* Compiler implementation of the D programming language
 * Copyright (C) 1999-2024 by The D Language Foundation, All Rights Reserved
 * written by Walter Bright
 * https://www.digitalmars.com
 * Distributed under the Boost Software License, Version 1.0.
 * https://www.boost.org/LICENSE_1_0.txt
 * https://github.com/dlang/dmd/blob/master/src/dmd/doc.h
 */

#pragma once

#include "root/dcompat.h" // for d_size_t

class Module;
class ErrorSink;

void gendocfile(Module *m, const char *ddoctext_ptr, d_size_t ddoctext_length,
                const char *datetime, ErrorSink *eSink, OutBuffer &outbuf);
