
/* Compiler implementation of the D programming language
 * Copyright (C) 2003-2019 by The D Language Foundation, All Rights Reserved
 * written by Walter Bright
 * http://www.digitalmars.com
 * Distributed under the Boost Software License, Version 1.0.
 * http://www.boost.org/LICENSE_1_0.txt
 * https://github.com/D-Programming-Language/dmd/blob/master/src/root/checkedint.h
 */

#include "dsystem.h"


int adds(int x, int y, bool& overflow);
int64_t adds(int64_t x, int64_t y, bool& overflow);
unsigned addu(unsigned x, unsigned y, bool& overflow);
uint64_t addu(uint64_t x, uint64_t y, bool& overflow);

int subs(int x, int y, bool& overflow);
int64_t subs(int64_t x, int64_t y, bool& overflow);
unsigned subu(unsigned x, unsigned y, bool& overflow);
uint64_t subu(uint64_t x, uint64_t y, bool& overflow);

int negs(int x, bool& overflow);
int64_t negs(int64_t x, bool& overflow);

int muls(int x, int y, bool& overflow);
int64_t muls(int64_t x, int64_t y, bool& overflow);
unsigned mulu(unsigned x, unsigned y, bool& overflow);
uint64_t mulu(uint64_t x, uint64_t y, bool& overflow);
