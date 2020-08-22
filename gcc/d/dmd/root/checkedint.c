
/**********************************************
 * This module implements integral arithmetic primitives that check
 * for out-of-range results.
 * This is a translation to C++ of D's core.checkedint
 *
 * Integral arithmetic operators operate on fixed width types.
 * Results that are not representable in those fixed widths are silently
 * truncated to fit.
 * This module offers integral arithmetic primitives that produce the
 * same results, but set an 'overflow' flag when such truncation occurs.
 * The setting is sticky, meaning that numerous operations can be cascaded
 * and then the flag need only be checked at the end.
 * Whether the operation is signed or unsigned is indicated by an 's' or 'u'
 * suffix, respectively. While this could be achieved without such suffixes by
 * using overloading on the signedness of the types, the suffix makes it clear
 * which is happening without needing to examine the types.
 *
 * While the generic versions of these functions are computationally expensive
 * relative to the cost of the operation itself, compiler implementations are free
 * to recognize them and generate equivalent and faster code.
 *
 * References: $(LINK2 http://blog.regehr.org/archives/1139, Fast Integer Overflow Checks)
 * Copyright: Copyright (C) 2014-2020 by The D Language Foundation, All Rights Reserved
 * License:   $(LINK2 http://www.boost.org/LICENSE_1_0.txt, Boost License 1.0)
 * Authors:   Walter Bright
 * Source:    https://github.com/D-Programming-Language/dmd/blob/master/src/root/checkedint.c
 */

#include "dsystem.h"
#include "checkedint.h"


/*******************************
 * Add two signed integers, checking for overflow.
 *
 * The overflow is sticky, meaning a sequence of operations can
 * be done and overflow need only be checked at the end.
 * Params:
 *      x = left operand
 *      y = right operand
 *      overflow = set if an overflow occurs, is not affected otherwise
 * Returns:
 *      the sum
 */

int adds(int x, int y, bool& overflow)
{
    int64_t r = (int64_t)x + (int64_t)y;
    if (r < INT32_MIN || r > INT32_MAX)
        overflow = true;
    return (int)r;
}

/// ditto
int64_t adds(int64_t x, int64_t y, bool& overflow)
{
    int64_t r = (uint64_t)x + (uint64_t)y;
    if ((x <  0 && y <  0 && r >= 0) ||
        (x >= 0 && y >= 0 && r <  0))
        overflow = true;
    return r;
}

/*******************************
 * Add two unsigned integers, checking for overflow (aka carry).
 *
 * The overflow is sticky, meaning a sequence of operations can
 * be done and overflow need only be checked at the end.
 * Params:
 *      x = left operand
 *      y = right operand
 *      overflow = set if an overflow occurs, is not affected otherwise
 * Returns:
 *      the sum
 */

unsigned addu(unsigned x, unsigned y, bool& overflow)
{
    unsigned r = x + y;
    if (r < x || r < y)
        overflow = true;
    return r;
}

/// ditto
uint64_t addu(uint64_t x, uint64_t y, bool& overflow)
{
    uint64_t r = x + y;
    if (r < x || r < y)
        overflow = true;
    return r;
}

/*******************************
 * Subtract two signed integers, checking for overflow.
 *
 * The overflow is sticky, meaning a sequence of operations can
 * be done and overflow need only be checked at the end.
 * Params:
 *      x = left operand
 *      y = right operand
 *      overflow = set if an overflow occurs, is not affected otherwise
 * Returns:
 *      the sum
 */

int subs(int x, int y, bool& overflow)
{
    int64_t r = (int64_t)x - (int64_t)y;
    if (r < INT32_MIN || r > INT32_MAX)
        overflow = true;
    return (int)r;
}

/// ditto
int64_t subs(int64_t x, int64_t y, bool& overflow)
{
    int64_t r = (uint64_t)x - (uint64_t)y;
    if ((x <  0 && y >= 0 && r >= 0) ||
        (x >= 0 && y <  0 && (r <  0 || y == INT64_MIN)))
        overflow = true;
    return r;
}

/*******************************
 * Subtract two unsigned integers, checking for overflow (aka borrow).
 *
 * The overflow is sticky, meaning a sequence of operations can
 * be done and overflow need only be checked at the end.
 * Params:
 *      x = left operand
 *      y = right operand
 *      overflow = set if an overflow occurs, is not affected otherwise
 * Returns:
 *      the sum
 */

unsigned subu(unsigned x, unsigned y, bool& overflow)
{
    if (x < y)
        overflow = true;
    return x - y;
}

/// ditto
uint64_t subu(uint64_t x, uint64_t y, bool& overflow)
{
    if (x < y)
        overflow = true;
    return x - y;
}

/***********************************************
 * Negate an integer.
 *
 * Params:
 *      x = operand
 *      overflow = set if x cannot be negated, is not affected otherwise
 * Returns:
 *      the negation of x
 */

int negs(int x, bool& overflow)
{
    if (x == (int)INT32_MIN)
        overflow = true;
    return -x;
}

/// ditto
int64_t negs(int64_t x, bool& overflow)
{
    if (x == INT64_MIN)
        overflow = true;
    return -x;
}

/*******************************
 * Multiply two signed integers, checking for overflow.
 *
 * The overflow is sticky, meaning a sequence of operations can
 * be done and overflow need only be checked at the end.
 * Params:
 *      x = left operand
 *      y = right operand
 *      overflow = set if an overflow occurs, is not affected otherwise
 * Returns:
 *      the sum
 */

int muls(int x, int y, bool& overflow)
{
    int64_t r = (int64_t)x * (int64_t)y;
    if (r < INT32_MIN || r > INT32_MAX)
        overflow = true;
    return (int)r;
}

/// ditto
int64_t muls(int64_t x, int64_t y, bool& overflow)
{
    int64_t r = (uint64_t)x * (uint64_t)y;
    int64_t not0or1 = ~(int64_t)1;
    if ((x & not0or1) && ((r == y) ? r : (r / x) != y))
        overflow = true;
    return r;
}

/*******************************
 * Multiply two unsigned integers, checking for overflow (aka carry).
 *
 * The overflow is sticky, meaning a sequence of operations can
 * be done and overflow need only be checked at the end.
 * Params:
 *      x = left operand
 *      y = right operand
 *      overflow = set if an overflow occurs, is not affected otherwise
 * Returns:
 *      the sum
 */

unsigned mulu(unsigned x, unsigned y, bool& overflow)
{
    uint64_t r = (uint64_t)x * (uint64_t)y;
    if (r > UINT32_MAX)
        overflow = true;
    return (unsigned)r;
}

/// ditto
uint64_t mulu(uint64_t x, uint64_t y, bool& overflow)
{
    uint64_t r = x * y;
    if (x && (r / x) != y)
        overflow = true;
    return r;
}
