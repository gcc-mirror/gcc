
/* Copyright (C) 2000-2019 by The D Language Foundation, All Rights Reserved
 * http://www.digitalmars.com
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE or copy at http://www.boost.org/LICENSE_1_0.txt)
 * https://github.com/D-Programming-Language/dmd/blob/master/src/root/rmem.c
 */

#include "dsystem.h"
#include "rmem.h"

/* This implementation of the storage allocator uses the standard C allocation package.
 */

Mem mem;

char *Mem::xstrdup(const char *s)
{
    char *p;

    if (s)
    {
#ifdef IN_GCC
        p = ::xstrdup(s);
#else
        p = strdup(s);
#endif
        if (p)
            return p;
        error();
    }
    return NULL;
}

void *Mem::xmalloc(size_t size)
{   void *p;

    if (!size)
        p = NULL;
    else
    {
#ifdef IN_GCC
        p = ::xmalloc(size);
#else
        p = malloc(size);
#endif
        if (!p)
            error();
    }
    return p;
}

void *Mem::xcalloc(size_t size, size_t n)
{   void *p;

    if (!size || !n)
        p = NULL;
    else
    {
#ifdef IN_GCC
        p = ::xcalloc(size, n);
#else
        p = calloc(size, n);
#endif
        if (!p)
            error();
    }
    return p;
}

void *Mem::xrealloc(void *p, size_t size)
{
    if (!size)
    {   if (p)
        {
            free(p);
            p = NULL;
        }
    }
    else if (!p)
    {
#ifdef IN_GCC
        p = ::xmalloc(size);
#else
        p = malloc(size);
#endif
        if (!p)
            error();
    }
    else
    {
        void *psave = p;
#ifdef IN_GCC
        p = ::xrealloc(psave, size);
#else
        p = realloc(psave, size);
#endif
        if (!p)
        {   xfree(psave);
            error();
        }
    }
    return p;
}

void Mem::xfree(void *p)
{
    if (p)
        free(p);
}

void *Mem::xmallocdup(void *o, size_t size)
{   void *p;

    if (!size)
        p = NULL;
    else
    {
#ifdef IN_GCC
        p = ::xmalloc(size);
#else
        p = malloc(size);
#endif
        if (!p)
            error();
        else
            memcpy(p,o,size);
    }
    return p;
}

void Mem::error()
{
    printf("Error: out of memory\n");
    exit(EXIT_FAILURE);
}

/* =================================================== */

/* Allocate, but never release
 */

// Allocate a little less than 1Mb because the C runtime adds some overhead that
// causes the actual memory block to be larger than 1Mb otherwise.
#define CHUNK_SIZE (256 * 4096 - 64)

static size_t heapleft = 0;
static void *heapp;

extern "C" void *allocmemory(size_t m_size)
{
    // 16 byte alignment is better (and sometimes needed) for doubles
    m_size = (m_size + 15) & ~15;

    // The layout of the code is selected so the most common case is straight through
    if (m_size <= heapleft)
    {
     L1:
        heapleft -= m_size;
        void *p = heapp;
        heapp = (void *)((char *)heapp + m_size);
        return p;
    }

    if (m_size > CHUNK_SIZE)
    {
#ifdef IN_GCC
        void *p = xmalloc(m_size);
#else
        void *p = malloc(m_size);
#endif
        if (p)
            return p;
        printf("Error: out of memory\n");
        exit(EXIT_FAILURE);
        return p;
    }

    heapleft = CHUNK_SIZE;
#ifdef IN_GCC
    heapp = xmalloc(CHUNK_SIZE);
#else
    heapp = malloc(CHUNK_SIZE);
#endif
    if (!heapp)
    {
        printf("Error: out of memory\n");
        exit(EXIT_FAILURE);
    }
    goto L1;
}
