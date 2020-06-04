/* Copyright (C) 2011-2020 by The D Language Foundation, All Rights Reserved
 * written by Walter Bright
 * http://www.digitalmars.com
 * Distributed under the Boost Software License, Version 1.0.
 * http://www.boost.org/LICENSE_1_0.txt
 * https://github.com/dlang/dmd/blob/master/src/dmd/root/bitarray.h
 */

#pragma once

#include "dsystem.h"
#include "object.h"
#include "rmem.h"

struct BitArray
{
    BitArray()
      : len(0)
      , ptr(NULL)
    {}

    ~BitArray()
    {
        mem.xfree(ptr);
    }

    d_size_t len;
    d_size_t *ptr;

private:
    BitArray(const BitArray&);
};
