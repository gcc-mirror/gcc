
/* Copyright (C) 2010-2021 by The D Language Foundation, All Rights Reserved
 * http://www.digitalmars.com
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE or copy at http://www.boost.org/LICENSE_1_0.txt)
 * https://github.com/D-Programming-Language/dmd/blob/master/src/root/aav.c
 */

/**
 * Implementation of associative arrays.
 *
 */

#include "dsystem.h"
#include "aav.h"
#include "rmem.h"


inline size_t hash(size_t a)
{
    a ^= (a >> 20) ^ (a >> 12);
    return a ^ (a >> 7) ^ (a >> 4);
}

struct aaA
{
    aaA *next;
    Key key;
    Value value;
};

struct AA
{
    aaA* *b;
    size_t b_length;
    size_t nodes;       // total number of aaA nodes
    aaA* binit[4];      // initial value of b[]

    aaA aafirst;        // a lot of these AA's have only one entry
};

/****************************************************
 * Determine number of entries in associative array.
 */

size_t dmd_aaLen(AA* aa)
{
    return aa ? aa->nodes : 0;
}


/*************************************************
 * Get pointer to value in associative array indexed by key.
 * Add entry for key if it is not already there, returning a pointer to a null Value.
 * Create the associative array if it does not already exist.
 */

Value* dmd_aaGet(AA** paa, Key key)
{
    //printf("paa = %p\n", paa);

    if (!*paa)
    {   AA *a = (AA *)mem.xmalloc(sizeof(AA));
        a->b = (aaA**)a->binit;
        a->b_length = 4;
        a->nodes = 0;
        a->binit[0] = NULL;
        a->binit[1] = NULL;
        a->binit[2] = NULL;
        a->binit[3] = NULL;
        *paa = a;
        assert((*paa)->b_length == 4);
    }
    //printf("paa = %p, *paa = %p\n", paa, *paa);

    assert((*paa)->b_length);
    size_t i = hash((size_t)key) & ((*paa)->b_length - 1);
    aaA** pe = &(*paa)->b[i];
    aaA *e;
    while ((e = *pe) != NULL)
    {
        if (key == e->key)
            return &e->value;
        pe = &e->next;
    }

    // Not found, create new elem
    //printf("create new one\n");

    size_t nodes = ++(*paa)->nodes;
    e = (nodes != 1) ? (aaA *)mem.xmalloc(sizeof(aaA)) : &(*paa)->aafirst;
    //e = new aaA();
    e->next = NULL;
    e->key = key;
    e->value = NULL;
    *pe = e;

    //printf("length = %d, nodes = %d\n", (*paa)->b_length, nodes);
    if (nodes > (*paa)->b_length * 2)
    {
        //printf("rehash\n");
        dmd_aaRehash(paa);
    }

    return &e->value;
}


/*************************************************
 * Get value in associative array indexed by key.
 * Returns NULL if it is not already there.
 */

Value dmd_aaGetRvalue(AA* aa, Key key)
{
    //printf("_aaGetRvalue(key = %p)\n", key);
    if (aa)
    {
        size_t i;
        size_t len = aa->b_length;
        i = hash((size_t)key) & (len-1);
        aaA* e = aa->b[i];
        while (e)
        {
            if (key == e->key)
                return e->value;
            e = e->next;
        }
    }
    return NULL;    // not found
}


/********************************************
 * Rehash an array.
 */

void dmd_aaRehash(AA** paa)
{
    //printf("Rehash\n");
    if (*paa)
    {
        AA *aa = *paa;
        if (aa)
        {
            size_t len = aa->b_length;
            if (len == 4)
                len = 32;
            else
                len *= 4;
            aaA** newb = (aaA**)mem.xmalloc(sizeof(aaA)*len);
            memset(newb, 0, len * sizeof(aaA*));

            for (size_t k = 0; k < aa->b_length; k++)
            {   aaA *e = aa->b[k];
                while (e)
                {   aaA* enext = e->next;
                    size_t j = hash((size_t)e->key) & (len-1);
                    e->next = newb[j];
                    newb[j] = e;
                    e = enext;
                }
            }
            if (aa->b != (aaA**)aa->binit)
                mem.xfree(aa->b);

            aa->b = newb;
            aa->b_length = len;
        }
    }
}
