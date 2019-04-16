
/* Copyright (C) 2010-2019 by The D Language Foundation, All Rights Reserved
 * http://www.digitalmars.com
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE or copy at http://www.boost.org/LICENSE_1_0.txt)
 * https://github.com/D-Programming-Language/dmd/blob/master/src/root/aav.h
 */

#pragma once

#include "dsystem.h"

typedef void* Value;
typedef void* Key;

struct AA;

size_t dmd_aaLen(AA* aa);
Value* dmd_aaGet(AA** aa, Key key);
Value dmd_aaGetRvalue(AA* aa, Key key);
void dmd_aaRehash(AA** paa);

