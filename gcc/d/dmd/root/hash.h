/**
 * Compiler implementation of the D programming language
 * http://dlang.org
 *
 * Copyright: Copyright (C) 1999-2019 by The D Language Foundation, All Rights Reserved
 * Authors:   Martin Nowak, Walter Bright, http://www.digitalmars.com
 * License:   $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Source:    $(DMDSRC root/_hash.h)
 */

#pragma once

#include "dsystem.h"                    // uint{8|16|32}_t

// MurmurHash2 was written by Austin Appleby, and is placed in the public
// domain. The author hereby disclaims copyright to this source code.
// https://sites.google.com/site/murmurhash/
static inline uint32_t calcHash(const uint8_t *data, size_t len)
{
    // 'm' and 'r' are mixing constants generated offline.
    // They're not really 'magic', they just happen to work well.

    const uint32_t m = 0x5bd1e995;
    const int r = 24;

    // Initialize the hash to a 'random' value

    uint32_t h = (uint32_t)len;

    // Mix 4 bytes at a time into the hash

    while(len >= 4)
    {
        uint32_t k = data[3] << 24 | data[2] << 16 | data[1] << 8 | data[0];

        k *= m;
        k ^= k >> r;
        k *= m;

        h *= m;
        h ^= k;

        data += 4;
        len -= 4;
    }

    // Handle the last few bytes of the input array

    switch(len & 3)
    {
    case 3: h ^= data[2] << 16; /* fall through */
    case 2: h ^= data[1] << 8;  /* fall through */
    case 1: h ^= data[0];
        h *= m;
    }

    // Do a few final mixes of the hash to ensure the last few
    // bytes are well-incorporated.

    h ^= h >> 13;
    h *= m;
    h ^= h >> 15;

    return h;
}

static inline uint32_t calcHash(const char *data, size_t len)
{
    return calcHash((const uint8_t *)data, len);
}

// combine and mix two words (boost::hash_combine)
static inline size_t mixHash(size_t h, size_t k)
{
    return h ^ (k + 0x9e3779b9 + (h << 6) + (h >> 2));
}
