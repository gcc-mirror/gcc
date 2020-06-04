
/* Copyright (C) 2010-2020 by The D Language Foundation, All Rights Reserved
 * http://www.digitalmars.com
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE or copy at http://www.boost.org/LICENSE_1_0.txt)
 * https://github.com/D-Programming-Language/dmd/blob/master/src/root/speller.c
 */

#include "dsystem.h"
#include "speller.h"

const char idchars[] = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_";

/**************************************************
 * combine a new result from the spell checker to
 * find the one with the closest symbol with
 * respect to the cost defined by the search function
 * Input/Output:
 *      p       best found spelling (NULL if none found yet)
 *      cost    cost of p (INT_MAX if none found yet)
 * Input:
 *      np      new found spelling (NULL if none found)
 *      ncost   cost of np if non-NULL
 * Returns:
 *      true    if the cost is less or equal 0
 *      false   otherwise
 */
bool combineSpellerResult(void*& p, int& cost, void* np, int ncost)
{
    if (np && ncost < cost)
    {
        p = np;
        cost = ncost;
        if (cost <= 0)
            return true;
    }
    return false;
}

void *spellerY(const char *seed, size_t seedlen, fp_speller_t fp, void *fparg,
        const char *charset, size_t index, int* cost)
{
    if (!seedlen)
        return NULL;
    assert(seed[seedlen] == 0);

    char tmp[30];
    char *buf;
    if (seedlen <= sizeof(tmp) - 2)
        buf = tmp;
    else
    {
        buf = (char *)alloca(seedlen + 2);    // leave space for extra char
        if (!buf)
            return NULL;                      // no matches
    }

    memcpy(buf, seed, index);
    *cost = INT_MAX;
    void* p = NULL;
    int ncost = 0;

    /* Delete at seed[index] */
    if (index < seedlen)
    {
        memcpy(buf + index, seed + index + 1, seedlen - index);
        assert(buf[seedlen - 1] == 0);
        void* np = (*fp)(fparg, buf, &ncost);
        if (combineSpellerResult(p, *cost, np, ncost))
            return p;
    }

    if (charset && *charset)
    {
        /* Substitutions */
        if (index < seedlen)
        {
            memcpy(buf, seed, seedlen + 1);
            for (const char *s = charset; *s; s++)
            {
                buf[index] = *s;

                //printf("sub buf = '%s'\n", buf);
                void* np = (*fp)(fparg, buf, &ncost);
                if (combineSpellerResult(p, *cost, np, ncost))
                    return p;
            }
            assert(buf[seedlen] == 0);
        }

        /* Insertions */
        memcpy (buf + index + 1, seed + index, seedlen + 1 - index);

        for (const char *s = charset; *s; s++)
        {
            buf[index] = *s;

            //printf("ins buf = '%s'\n", buf);
            void* np = (*fp)(fparg, buf, &ncost);
            if (combineSpellerResult(p, *cost, np, ncost))
                return p;
        }
        assert(buf[seedlen + 1] == 0);
    }

    return p;                // return "best" result
}

void *spellerX(const char *seed, size_t seedlen, fp_speller_t fp, void *fparg,
        const char *charset, int flag)
{
    if (!seedlen)
        return NULL;

    char tmp[30];
    char *buf;
    if (seedlen <= sizeof(tmp) - 2)
        buf = tmp;
    else
    {
        buf = (char *)alloca(seedlen + 2);    // leave space for extra char
        if (!buf)
            return NULL;                      // no matches
    }
    int cost = INT_MAX, ncost = 0;
    void *p = NULL, *np;

    /* Deletions */
    memcpy(buf, seed + 1, seedlen);
    for (size_t i = 0; i < seedlen; i++)
    {
        //printf("del buf = '%s'\n", buf);
        if (flag)
            np = spellerY(buf, seedlen - 1, fp, fparg, charset, i, &ncost);
        else
            np = (*fp)(fparg, buf, &ncost);
        if (combineSpellerResult(p, cost, np, ncost))
            return p;

        buf[i] = seed[i];
    }

    /* Transpositions */
    if (!flag)
    {
        memcpy(buf, seed, seedlen + 1);
        for (size_t i = 0; i + 1 < seedlen; i++)
        {
            // swap [i] and [i + 1]
            buf[i] = seed[i + 1];
            buf[i + 1] = seed[i];

            //printf("tra buf = '%s'\n", buf);
            if (combineSpellerResult(p, cost, (*fp)(fparg, buf, &ncost), ncost))
                return p;

            buf[i] = seed[i];
        }
    }

    if (charset && *charset)
    {
        /* Substitutions */
        memcpy(buf, seed, seedlen + 1);
        for (size_t i = 0; i < seedlen; i++)
        {
            for (const char *s = charset; *s; s++)
            {
                buf[i] = *s;

                //printf("sub buf = '%s'\n", buf);
                if (flag)
                    np = spellerY(buf, seedlen, fp, fparg, charset, i + 1, &ncost);
                else
                    np = (*fp)(fparg, buf, &ncost);
                if (combineSpellerResult(p, cost, np, ncost))
                    return p;
            }
            buf[i] = seed[i];
        }

        /* Insertions */
        memcpy(buf + 1, seed, seedlen + 1);
        for (size_t i = 0; i <= seedlen; i++)      // yes, do seedlen+1 iterations
        {
            for (const char *s = charset; *s; s++)
            {
                buf[i] = *s;

                //printf("ins buf = '%s'\n", buf);
                if (flag)
                    np = spellerY(buf, seedlen + 1, fp, fparg, charset, i + 1, &ncost);
                else
                    np = (*fp)(fparg, buf, &ncost);
                if (combineSpellerResult(p, cost, np, ncost))
                    return p;
            }
            buf[i] = seed[i];   // going past end of seed[] is ok, as we hit the 0
        }
    }

    return p;                // return "best" result
}

/**************************************************
 * Looks for correct spelling.
 * Currently only looks a 'distance' of one from the seed[].
 * This does an exhaustive search, so can potentially be very slow.
 * Input:
 *      seed            wrongly spelled word
 *      fp              search function
 *      fparg           argument to search function
 *      charset         character set
 * Returns:
 *      NULL            no correct spellings found
 *      void*           value returned by fp() for first possible correct spelling
 */

void *speller(const char *seed, fp_speller_t fp, void *fparg, const char *charset)
{
    size_t seedlen = strlen(seed);
    size_t maxdist = seedlen < 4 ? seedlen / 2 : 2;
    for (size_t distance = 0; distance < maxdist; distance++)
    {   void *p = spellerX(seed, seedlen, fp, fparg, charset, distance);
        if (p)
            return p;
//      if (seedlen > 10)
//          break;
    }
    return NULL;   // didn't find it
}
