
/* Copyright (C) 1999-2019 by The D Language Foundation, All Rights Reserved
 * written by Walter Bright
 * http://www.digitalmars.com
 * Distributed under the Boost Software License, Version 1.0.
 * http://www.boost.org/LICENSE_1_0.txt
 * https://github.com/dlang/dmd/blob/master/src/dmd/root/dcompat.h
 */

#pragma once

#include "dsystem.h"

/// Represents a D [ ] array
template<typename T>
struct DArray
{
    size_t length;
    T *ptr;
};
