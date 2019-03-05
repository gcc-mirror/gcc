
/* Copyright (C) 2010-2019 by The D Language Foundation, All Rights Reserved
 * http://www.digitalmars.com
 * Distributed under the Boost Software License, Version 1.0.
 * (See accompanying file LICENSE or copy at http://www.boost.org/LICENSE_1_0.txt)
 * https://github.com/D-Programming-Language/dmd/blob/master/src/root/speller.h
 */

#pragma once

typedef void *(fp_speller_t)(void *, const char *, int*);

extern const char idchars[];

void *speller(const char *seed, fp_speller_t fp, void *fparg, const char *charset);

