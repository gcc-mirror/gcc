
/* Copyright (C) 1999-2019 by The D Language Foundation, All Rights Reserved
 * http://www.digitalmars.com
 * Distributed under the Boost Software License, Version 1.0.
 * http://www.boost.org/LICENSE_1_0.txt
 * https://github.com/dlang/dmd/blob/master/src/dmd/root/rmem.h
 */

#pragma once

#include "dsystem.h"    // for size_t

#if __APPLE__ && __i386__
    /* size_t is 'unsigned long', which makes it mangle differently
     * than D's 'uint'
     */
    typedef unsigned d_size_t;
#else
    typedef size_t d_size_t;
#endif

struct Mem
{
    Mem() { }

    static char *xstrdup(const char *s);
    static void *xmalloc(d_size_t size);
    static void *xcalloc(d_size_t size, d_size_t n);
    static void *xrealloc(void *p, d_size_t size);
    static void xfree(void *p);
    static void *xmallocdup(void *o, d_size_t size);
    static void error();
};

extern Mem mem;
