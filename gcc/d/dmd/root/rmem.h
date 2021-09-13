
/* Copyright (C) 1999-2021 by The D Language Foundation, All Rights Reserved
 * http://www.digitalmars.com
 * Distributed under the Boost Software License, Version 1.0.
 * http://www.boost.org/LICENSE_1_0.txt
 * https://github.com/dlang/dmd/blob/master/src/dmd/root/rmem.h
 */

#pragma once

#include "dsystem.h"    // for size_t

struct Mem
{
    Mem() { }

    static char *xstrdup(const char *s);
    static void *xmalloc(size_t size);
    static void *xcalloc(size_t size, size_t n);
    static void *xrealloc(void *p, size_t size);
    static void xfree(void *p);
    static void *xmallocdup(void *o, size_t size);
    static void error();
};

extern Mem mem;
